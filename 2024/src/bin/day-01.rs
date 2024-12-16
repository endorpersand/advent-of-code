fn main() {
    let input = std::fs::read_to_string("inputs/01.txt").unwrap();
    soln(&input);
}

fn parse(input: &str) -> (Vec<usize>, Vec<usize>) {
    input.lines()
        .map(|l| l.split_once("   ").unwrap())
        .map(|(left, right)| (left.parse::<usize>().unwrap(), right.parse::<usize>().unwrap()))
        .collect()
}
fn soln(input: &str) {
    let (mut a, mut b) = parse(input);

    // pt 1
    a.sort();
    b.sort();
    let p1: usize = std::iter::zip(&*a, &*b).map(|(&a, &b)| b.abs_diff(a)).sum();
    println!("{p1}");

    // pt 2
    let p2: usize = a.iter().map(|av| b.iter().filter(|&bv| bv == av).count() * av).sum();
    println!("{p2}");
}