use criterion::{criterion_group, criterion_main, Criterion};
use aoc_2024::{d6p1, d6p2};

pub fn day06(c: &mut Criterion) {
    let input = std::fs::read_to_string("inputs/06.txt").unwrap();
    c.bench_function("test p1", |b| b.iter(|| d6p1(&input)));
    c.bench_function("test p2", |b| b.iter(|| d6p2(&input)));
}

criterion_group!(benches, day06);
criterion_main!(benches);