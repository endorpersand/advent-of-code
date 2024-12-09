use aoc_2024::optimized::day_06;
use criterion::{criterion_group, criterion_main, Criterion};

pub fn day_06(c: &mut Criterion) {
    let input = std::fs::read_to_string("inputs/06.txt").unwrap();
    c.bench_function("test d6p1", |b| b.iter(|| day_06::part1(&input)));
    c.bench_function("test d6p2", |b| b.iter(|| day_06::part2(&input)));
}

criterion_group!(benches, day_06);
criterion_main!(benches);