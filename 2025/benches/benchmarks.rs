use std::hint::black_box;

use aoc_2025::optimized::*;
use criterion::{criterion_group, criterion_main, Criterion};

pub fn day_03(c: &mut Criterion) {
    let input = std::fs::read_to_string("inputs/03.txt").unwrap();
    c.bench_function("test d03p1", |b| b.iter(|| day_03::part1(black_box(&input))));
    c.bench_function("test d03p2", |b| b.iter(|| day_03::part2(black_box(&input))));
}

criterion_group!(benches, day_03);
criterion_main!(benches);