// For original, see day-05a.rs and day-06a.rs

use std::collections::BTreeMap;
use std::ops::{Range, Bound};

fn main() {
    let txt = std::fs::read_to_string("inputs/05.txt").unwrap();
    let State { seeds, mappers } = parse(&txt);

    let mloc = seeds.iter()
        .copied()
        .map(|s| mappers.iter().fold(s, |acc, cv| cv.map(acc)))
        .min();
    println!("{mloc:?}");

    let seed_ranges: Vec<_> = seeds.chunks(2)
        .map(|s| s[0] .. (s[0] + s[1]))
        .collect();
    let mut seed_frontier = seed_ranges.clone();
    for m in &mappers {
        seed_frontier = seed_frontier.into_iter()
            .flat_map(|r| {
                let Range { start, end } = r.clone();
                let partition_points = m.map.range((Bound::Excluded(start), Bound::Excluded(end)))
                    .map(|(&k, _)| k);

                let mut points = vec![start];
                points.extend(partition_points);
                points.push(end);

                points.windows(2)
                    .map(|s| s[0] .. s[1])
                    .collect::<Vec<_>>()
            })
            .map(|r| m.map_range(r))
            .collect()
    }
    
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
            mappers.push(BTreeMap::from_iter([(0, 0)]));
        } else if line.starts_with(char::is_numeric) {
            let values: Vec<_> = line.split_whitespace()
                .map(|s| s.parse::<usize>().unwrap())
                .collect();
            
            let dest = values[0];
            let src = values[1];
            let len = values[2];

            let map = mappers.last_mut().unwrap();
            map.insert(src, (dest as isize) - (src as isize));
            map.entry(src + len).or_insert(0);
        }
    }

    let mappers = mappers.into_iter()
        .map(|map| Mapper { map })
        .collect();
    State { seeds, mappers }
}
#[derive(Debug)]
struct Mapper {
    map: BTreeMap<usize, isize>
}
impl Mapper {
    fn map(&self, input: usize) -> usize {
        let (_, &delta) = self.map.range(..=input)
            .next_back()
            .unwrap();
        input.wrapping_add_signed(delta)
    }

    fn map_range(&self, input: Range<usize>) -> Range<usize> {
        let (_, &delta) = self.map.range(..=input.start)
            .next_back()
            .unwrap();
        input.start.wrapping_add_signed(delta) .. input.end.wrapping_add_signed(delta)
    }
}