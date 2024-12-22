use std::collections::{HashMap, HashSet};

fn main() {
    let input = std::fs::read_to_string("inputs/22.txt").unwrap();
    soln(&input);
}

#[allow(clippy::let_and_return)]
fn next_secret(secret: usize) -> usize {
    let secret = (secret ^ (secret <<  6)) & 0xFFFFFF;
    let secret = (secret ^ (secret >>  5)) & 0xFFFFFF;
    let secret = (secret ^ (secret << 11)) & 0xFFFFFF;
    secret
}

fn secrets_iter(secret: usize) -> impl Iterator<Item = usize> {
    std::iter::successors(Some(secret), |&n| Some(next_secret(n)))
}
fn all_quads(seq: &[usize]) -> impl DoubleEndedIterator<Item=([isize; 4], usize)> + use<'_> {
    (0..(seq.len() - 4))
        .map(|i| {
            let value = seq[i + 4] % 10;
            let group = std::array::from_fn(|j| {
                let s0 = seq[i + j + 1] % 10;
                let s1 = seq[i + j] % 10;
                s0 as isize - s1 as isize
            });
            (group, value)
        })
}
fn soln(input: &str) {
    let secrets: Vec<usize> = input.lines()
        .flat_map(str::parse)
        .collect();

    let p1: usize = secrets.iter()
        .map(|&n| secrets_iter(n).nth(2000).unwrap())
        .sum();
    println!("{p1}");

    let maps: Vec<HashMap<_, _>> = secrets.iter()
        .map(|&n| {
            let seq: Vec<_> = secrets_iter(n).take(2001).collect();
            all_quads(&seq).rev().collect()
        })
        .collect();

    let keys: HashSet<_> = maps.iter()
        .flat_map(|m| m.keys())
        .collect();
    let p2: usize = keys.into_iter()
        .map(|k| {
            maps.iter()
                .map(|m| m.get(k).map_or(0, |&n| n))
                .sum()
        })
        .max()
        .unwrap();
    
    println!("{p2}");
}