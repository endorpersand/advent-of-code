fn main() {
    let txt = std::fs::read_to_string("inputs/09.txt").unwrap();
    let State { data } = parse(&txt);

    let a: isize = data.iter()
        .map(|line| compute_next(line))
        .sum();
    println!("{a:?}");
    
    let a: isize = data.iter()
        .map(|line| compute_prev(line))
        .sum();
    println!("{a:?}");
}

#[derive(Debug)]
struct State {
    data: Vec<Vec<isize>>
}
fn parse(file: &str) -> State {
    let data = file.lines()
        .map(|line| {
            line.split_whitespace()
                .map(|lit| lit.parse().unwrap())
                .collect()
        })
        .collect();

    State { data }
}

fn compute_next(data: &[isize]) -> isize {
    let mut rhs = 0;

    let mut remaining = data.to_vec();
    while !remaining.is_empty() {
        rhs += remaining.last().unwrap();
        remaining = std::iter::zip(remaining.iter(), remaining.iter().skip(1))
            .map(|(&a, &b)| b - a)
            .collect();
    }

    rhs
}
fn compute_prev(data: &[isize]) -> isize {
    let mut lhs = 0;

    let mut remaining = data.to_vec();
    while !remaining.is_empty() {
        lhs = remaining.first().unwrap() - lhs;
        remaining = std::iter::zip(remaining.iter(), remaining.iter().skip(1))
            .map(|(&a, &b)| b - a)
            .collect();
    }

    lhs
}