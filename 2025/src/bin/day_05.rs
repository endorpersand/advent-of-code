use std::collections::BTreeMap;
use std::ops::{Range, RangeInclusive};

fn main() {
    let input = std::fs::read_to_string("inputs/05.txt").unwrap();
    soln(&input);
}

struct Input {
    ranges: Vec<RangeInclusive<usize>>,
    ingredients: Vec<usize>
}
fn parse(input: &str) -> Input {
    let mut lines = input.lines();
    let mut ranges = vec![];

    while let Some(line) = lines.next() && !line.is_empty() {
        let (l, r) = line.split_once('-').unwrap();
        ranges.push(l.parse().unwrap() ..= r.parse().unwrap())
    }
    let ingredients = lines.map(|s| s.parse().unwrap()).collect();

    Input { ranges, ingredients }
}

#[derive(Default)]
struct RangeMap(BTreeMap<usize, usize>);
impl RangeMap {
    fn ranges_overlapping(&self, r: &RangeInclusive<usize>) -> Vec<Range<usize>> {
        let mut v: Vec<_> = self.0.range(..=r.end())
            .rev()
            .map(|(&start, &len)| start .. start + len)
            .take_while(|mr| mr.start <= *r.end() && *r.start() < mr.end)
            .collect();
        v.reverse();
        v
    }
    fn add_range(&mut self, r: RangeInclusive<usize>) {
        let overlapping = self.ranges_overlapping(&r);
        match (overlapping.first(), overlapping.last()) {
            (Some(front), Some(back)) => {
                // If overlapping ranges exist, merge overlapping ranges
                let start = front.start.min(*r.start());
                let end = back.end.max(*r.end() + 1);
                
                for o in overlapping {
                    self.0.remove(&o.start);
                }
                self.0.insert(start, end - start);
            },
            _ => {
                // If no overlapping ranges, create new range
                self.0.insert(*r.start(), r.count());
            }
        }
    }
}
fn soln(input: &str) {
    let data = parse(input);

    // part 1
    let p1 = data.ingredients.iter()
        .filter(|i| data.ranges.iter().any(|r| r.contains(i)))
        .count();
    println!("{p1}");

    // part 2
    let mut map = RangeMap::default();
    for d in &data.ranges {
        map.add_range(d.clone());
    }
    let p2: usize = map.0.values().sum();
    println!("{p2}");
}