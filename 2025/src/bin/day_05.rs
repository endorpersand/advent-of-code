use std::ops::RangeInclusive;

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

fn ranges_overlapping(r1: &RangeInclusive<usize>, r2: &RangeInclusive<usize>) -> bool {
    r1.start() <= r2.end() && r2.start() <= r1.end()
}

fn soln(input: &str) {
    let mut data = parse(input);

    // part 1
    let p1 = data.ingredients.iter()
        .filter(|i| data.ranges.iter().any(|r| r.contains(i)))
        .count();
    println!("{p1}");

    // part 2
    data.ranges.sort_by_key(|r| *r.start());
    let mut ranges = vec![]; // A list of all ranges, merged
    for r in data.ranges {
        if let Some(lr) = ranges.last_mut() && ranges_overlapping(lr, &r) {
            // If this range overlaps with the previous range, merge
            let &start = lr.start().min(r.start());
            let &end = lr.end().max(r.end());
            *lr = start ..= end;
        } else {
            // Otherwise, add new range
            ranges.push(r);
        }
    }
    let p2: usize = ranges.into_iter()
        .map(|r| r.count())
        .sum();
    println!("{p2}");
}