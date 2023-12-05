use std::collections::BTreeMap;
use std::ops::Range;

fn main() {
    let txt = std::fs::read_to_string("inputs/05.txt").unwrap();
    let State { seeds, mappers } = parse(&txt);
    
    let seed_ranges: Vec<_> = seeds.chunks(2)
        .map(|s| s[0] .. (s[0] + s[1]))
        .collect();
    let mappers: Vec<_> = mappers.into_iter()
        .map(RangedMapper::from)
        .inspect(|s| println!("{s:?}"))
        .collect();
    
    let mut seed_frontier = seed_ranges.clone();
    for m in &mappers {
        seed_frontier = seed_frontier.into_iter()
            .flat_map(|r| {
                let partition_points = m.map.range(r.clone()).map(|(&k, _)| k);
                
                let mut points = Vec::from_iter((!m.map.contains_key(&r.start)).then_some(r.start));
                points.extend(partition_points);
                points.push(r.end);

                points.windows(2)
                    .map(|s| s[0] .. s[1])
                    .collect::<Vec<_>>()
            })
            .map(|r| m.map_range(r))
            .collect()
    }
    
    println!("{}", seed_frontier.len());
    println!("{:?}", seed_frontier.iter().map(|r| r.start).min());
}

#[derive(Debug)]
struct State {
    seeds: Vec<usize>,
    mappers: Vec<Mapper>
}
fn parse(file: &str) -> State {
    let mut lines = file.lines();
    
    let seeds_line = lines.next().unwrap();
    let (_, seeds_line) = seeds_line.split_once(": ").unwrap();
    let seeds = seeds_line.split_whitespace()
        .map(|s| s.parse().unwrap())
        .collect();

    let mut mappers = vec![];

    for line in lines {
        if line.starts_with(char::is_alphabetic) {
            mappers.push(vec![]);
        } else if line.starts_with(char::is_numeric) {
            let values: Vec<_> = line.split_whitespace()
                .map(|s| s.parse::<usize>().unwrap())
                .collect();
            
            let dest = values[0];
            let src = values[1];
            let len = values[2];

            mappers.last_mut().unwrap()
                .push((src..(src + len), (dest as isize) - (src as isize)));
        }
        
    }

    let mappers = mappers.into_iter()
        .map(|map| Mapper { map })
        .collect();
    State { seeds, mappers }
}

#[derive(Debug)]
struct Mapper {
    map: Vec<(Range<usize>, isize)>
}
#[derive(Debug)]
struct RangedMapper {
    map: BTreeMap<usize, isize>
}
impl From<Mapper> for RangedMapper {
    fn from(value: Mapper) -> Self {
        let Mapper { map } = value;

        let mut new_map = BTreeMap::from_iter([(0, 0)]);
        for (range, delta) in map {
            new_map.insert(range.start, delta);
            new_map.entry(range.end).or_insert(0);
        }
        Self { map: new_map }
    } 
}
impl RangedMapper {
    fn map_range(&self, input: Range<usize>) -> Range<usize> {
        let (_, &delta) = self.map.range(..=input.start)
            .next_back()
            .unwrap();
        input.start.wrapping_add_signed(delta) .. input.end.wrapping_add_signed(delta)
    }
}