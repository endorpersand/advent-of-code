use std::cmp;

fn main() {
    let input = std::fs::read_to_string("inputs/02.txt").unwrap();
    soln(&input);
}

fn parse(input: &str) -> Vec<Vec<usize>> {
    input.lines()
        .map(|s| {
            s.split_whitespace()
                .map(|s| s.parse::<usize>())
                .collect::<Result<Vec<_>, _>>()
                .unwrap()
        })
        .collect()
}

fn soln(input: &str) {
    let data = parse(input);

    // Single pass safe check
    fn is_safe(line: &[usize]) -> bool {
        let mut sign = cmp::Ordering::Equal;
        line.windows(2).all(|s| {
            let monotonic = match (sign, s[0].cmp(&s[1])) {
                (_, cmp::Ordering::Equal) => false,
                (cmp::Ordering::Equal, c) => { sign = c; true },
                (a, b) => a == b
            };
            
            monotonic && s[0].abs_diff(s[1]) <= 3
        })
    }

    let p1: usize = data.iter()
        .filter(|line| is_safe(line))
        .count();
    println!("{p1}");

    let p2: usize = data.iter()
    .filter(|&line| {
        (0..line.len()).any(|i| {
            let mut l = line.clone();
            l.remove(i);
            is_safe(&l)
        })
    })
    .count();

    println!("{p2}");
}