use std::sync::LazyLock;

use regex::Regex;

static PATTERN: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"(\d+)   (\d+)").unwrap());
fn main() {
    let input = std::fs::read_to_string("inputs/01.txt").unwrap();
    let (mut a, mut b): (Vec<_>, Vec<_>) = PATTERN.captures_iter(&input)
        .map(|c| (c.get(1).unwrap().as_str().parse::<usize>().unwrap(), c.get(2).unwrap().as_str().parse::<usize>().unwrap()))
        .unzip();

    soln1(&mut a, &mut b);
}

#[allow(unused)]
fn soln1(a: &mut [usize], b: &mut [usize]) {
    // pt 1
    a.sort();
    b.sort();
    let x: usize = std::iter::zip(&*a, &*b).map(|(&a, &b)| b.abs_diff(a)).sum();
    println!("{x}");

    // pt 2
    let y: usize = a.iter().map(|av| b.iter().filter(|&bv| bv == av).count() * av).sum();
    println!("{y}");
}