use std::collections::{HashMap, HashSet};

fn main() {
    let input = std::fs::read_to_string("inputs/22.txt").unwrap();
    soln(&input);
}

fn next_secret(secret: usize) -> usize {
    let mix = |secret, seed| secret ^ seed;
    let prune = |secret| secret % 16777216;

    let secret = prune(mix(secret, secret * 64));
    let secret = prune(mix(secret, secret / 32));
    prune(mix(secret, secret * 2048))
}

fn secrets_iter(secret: usize) -> impl Iterator<Item = usize> {
    std::iter::successors(Some(secret), |&n| Some(next_secret(n)))
}
fn secret2k(secret: usize) -> usize {
    secrets_iter(secret).nth(2000).unwrap()
}
fn find_fours(seq: &[usize]) -> Vec<(usize, [isize; 4])> {
    std::iter::zip(seq, &seq[4..])
        .enumerate()
        .map(|(i, _)| {
            let value = seq[i + 4] % 10;
            let group = std::array::from_fn(|j| {
                let s0 = seq[i + j + 1] % 10;
                let s1 = seq[i + j] % 10;
                s0 as isize - s1 as isize
            });
            (value, group)
        })
        .collect()
}
fn soln(input: &str) {
    let secrets: Vec<usize> = input.lines()
        .flat_map(|s| s.parse())
        .collect();

    let p1: usize = secrets.iter()
        .map(|&n| secret2k(n))
        .sum();
    println!("{p1}");

    let seqs: Vec<Vec<_>> = secrets.iter()
        .map(|&n| secrets_iter(n).take(2001).collect())
        .collect();

    
    let maps: Vec<_> = seqs.iter()
        .map(|s| {
            let mut m = HashMap::new();
            for (i, g) in find_fours(s) {
                m.entry(g).or_insert(i);
            }
            m
        })
        .collect();

    let keys: HashSet<_> = maps.iter()
        .flat_map(|m| m.keys())
        .collect();
    let p2 = keys.into_iter()
        .map(|&k| {
            maps.iter()
                .map(|m| *m.get(&k).unwrap_or(&0))
                .sum::<usize>()
        })
        .max()
        .unwrap();
    
    println!("{p2}");
}