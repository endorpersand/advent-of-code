use std::ops::Range;

fn main() {
    let txt = std::fs::read_to_string("inputs/05.txt").unwrap();
    let State { seeds, mappers } = dbg!(parse(&txt));

    let mloc = seeds.iter()
        .copied()
        .map(|s| mappers.iter().fold(s, |acc, cv| cv.map(acc)))
        .min();
    println!("{mloc:?}");
}

#[derive(Debug)]
struct Mapper {
    map: Vec<(Range<usize>, isize)>
}
impl Mapper {
    fn map(&self, input: usize) -> usize {
        self.map.iter()
            .find(|&(r, _)| r.contains(&input))
            .map_or(input, |&(_, delta)| ((input as isize) + delta) as usize)
    }
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