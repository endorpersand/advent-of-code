fn main() {
    let input = std::fs::read_to_string("inputs/01.txt").unwrap();
    let (mut a, mut b): (Vec<_>, Vec<_>) = input.lines()
        .map(|l| l.split_once("   ").unwrap())
        .map(|(left, right)| (left.parse::<usize>().unwrap(), right.parse::<usize>().unwrap()))
        .collect();

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