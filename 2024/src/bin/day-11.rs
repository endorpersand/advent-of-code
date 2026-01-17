use std::collections::HashMap;

fn main() {
    let input = std::fs::read_to_string("inputs/11.txt").unwrap();
    let data = parse(&input);

    println!("{}", exec(&data, 25));
    println!("{}", exec(&data, 75));
}

fn parse(input: &str) -> Vec<usize> {
    input.split_whitespace()
        .map(str::parse)
        .collect::<Result<_, _>>()
        .unwrap()
}
fn exec(input: &[usize], iterations: usize) -> usize {
    let mut ctr: HashMap<usize, usize> = input.iter()
        .map(|&s| (s, 1))
        .collect();

    fn insert(m: &mut HashMap<usize, usize>, keys: &[usize], n: usize) {
        keys.iter().copied().for_each(|k| *m.entry(k).or_default() += n);
    }
    for _ in 0..iterations {
        for (n, c) in std::mem::take(&mut ctr) {
            match n {
                0 => insert(&mut ctr, &[1], c),
                n if n.ilog10() % 2 == 1 => {
                    let e = n.ilog10().div_ceil(2);
                    insert(&mut ctr, &[n / 10usize.pow(e), n % 10usize.pow(e)], c)
                }
                n => insert(&mut ctr, &[n * 2024], c)
            }
        }
    }

    ctr.into_values().sum()
}