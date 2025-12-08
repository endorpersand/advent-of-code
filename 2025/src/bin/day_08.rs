use std::cmp::Reverse;
use std::collections::{BinaryHeap, HashMap};

use disjoint::DisjointSetVec;

fn main() {
    let input = std::fs::read_to_string("inputs/08.txt").unwrap();
    println!("{}", part1(&input));
    println!("{}", part2(&input));
}

type Vector = [usize; 3];

fn sq_dist(l: Vector, r: Vector) -> usize {
    std::iter::zip(l, r)
        .map(|(a, b)| a.abs_diff(b).pow(2))
        .sum()
}
fn parse(input: &str) -> Vec<[usize; 3]> {
    input.lines()
        .map(|l| {
            let b: Box<[_; 3]> = l.split(',')
                .map(|s| s.parse().unwrap())
                .collect::<Box<[_]>>()
                .try_into()
                .unwrap();

            *b
        }).collect()
}
fn part1(input: &str) -> usize {
    const SHORTEST_N: usize = 1000;
    const TOP_N: usize = 3;

    let mut vectors = parse(input);
    let revmap = HashMap::<_, usize>::from_iter(vectors.iter().copied().zip(0..));
    let mut distances = BinaryHeap::new();
    let mut jboxes = DisjointSetVec::from(vectors.clone());

    vectors.sort();
    for (i, &a) in vectors.iter().enumerate() {
        for &b in &vectors[i+1..] {
            let [a, b] = if a < b { [a, b] } else { [b, a] };
            distances.push((Reverse(sq_dist(a, b)), [a, b]));
        }
    }

    for _ in 0..SHORTEST_N {
        let Some((_, [a, b])) = distances.pop() else { break };
        jboxes.join(revmap[&a], revmap[&b]);
    }

    let mut sets = jboxes.indices().sets();
    sets.sort_by_key(|s| !s.len());
    println!("{sets:?}");
    sets[0..TOP_N].iter().map(|s| s.len()).product()
}
fn part2(input: &str) -> usize {
    let mut vectors = parse(input);
    let revmap = HashMap::<_, usize>::from_iter(vectors.iter().copied().zip(0..));
    let mut distances = BinaryHeap::new();
    let mut jboxes = DisjointSetVec::from(vectors.clone());

    vectors.sort();
    for (i, &a) in vectors.iter().enumerate() {
        for &b in &vectors[i+1..] {
            let [a, b] = if a < b { [a, b] } else { [b, a] };
            distances.push((Reverse(sq_dist(a, b)), [a, b]));
        }
    }

    fn count_sets<T>(v: &DisjointSetVec<T>) -> usize {
        (0..v.len())
            .filter(|&i| v.root_of(i) == i)
            .count()
    }

    while let Some((_, [a, b])) = distances.pop() {
        if jboxes.join(revmap[&a], revmap[&b]) && count_sets(&jboxes) == 1 {
            return a[0] * b[0];
        }
    }
    panic!("can't find");
}
