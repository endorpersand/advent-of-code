use std::collections::HashMap;

use once_cell::sync::Lazy;
use regex::Regex;

fn main() {
    let txt = std::fs::read_to_string("inputs/08.txt").unwrap();
    let State { dirs, paths } = parse(&txt);

    let mut dir_it = dirs.iter()
        .copied()
        .cycle()
        .enumerate();

    let mut current = *b"AAA";
    for (_, dir) in dir_it.by_ref() {
        current = paths[&current][if dir { 1 } else { 0 }];
        if &current == b"ZZZ" { break; }
    }
    let out = dir_it.next().unwrap().0;
    println!("{out}");

    let mut current2: Vec<_> = paths.keys().copied().filter(|[_, _, end]| end == &b'A').collect();
    let mut done = Vec::with_capacity(current2.len());

    let mut dir_it = dirs.iter()
        .copied()
        .cycle()
        .enumerate();
    for (step, dir) in dir_it.by_ref() {
        for curr in std::mem::take(&mut current2) {
            let next = paths[&curr][if dir { 1 } else { 0 }];
            if next[2] == b'Z' {
                done.push(step + 1);
            } else {
                current2.push(next);
            }
        }

        if current2.is_empty() { break; }
    }
    let out = done.iter().copied().fold(1, lcm);
    println!("{out}");
}

struct State {
    // left = false, right = true
    dirs: Vec<bool>,
    paths: HashMap<[u8; 3], [[u8; 3]; 2]>
}
fn copy_to_array<T: Copy, const N: usize>(s: &[T]) -> [T; N] {
    assert_eq!(s.len(), N);
    std::array::from_fn(|i| s[i])
}
fn parse(file: &str) -> State {
    static RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"(\w+) = \((\w+), (\w+)\)").unwrap());
    let mut lines = file.lines();
    
    let dir_str = lines.next().unwrap();
    let dirs = dir_str.as_bytes()
        .iter()
        .map(|&byte| match byte {
            b'L' => false,
            b'R' => true,
            _ => unreachable!()
        })
        .collect();

    lines.next();
    let mut paths = HashMap::new();
    for line in lines {
        let capture = RE.captures(line).unwrap();

        let start = copy_to_array(capture[1].as_bytes());
        let left  = copy_to_array(capture[2].as_bytes());
        let right = copy_to_array(capture[3].as_bytes());
        paths.insert(start, [left, right]);
    }

    State { dirs, paths }
}

fn gcd(mut a: usize, mut b: usize) -> usize {
    while b != 0 {
        [a, b] = [b, a % b];
    }
    a
}
fn lcm(a: usize, b: usize) -> usize {
    a * b / gcd(a, b)
}