use aoc_2024::optimized::*;
use criterion::{criterion_group, criterion_main, Criterion};

pub fn day_06(c: &mut Criterion) {
    let input = std::fs::read_to_string("inputs/06.txt").unwrap();
    c.bench_function("test d06p1", |b| b.iter(|| day_06::part1(&input)));
    c.bench_function("test d06p2", |b| b.iter(|| day_06::part2(&input)));
}

pub fn day_09(c: &mut Criterion) {
    let input = std::fs::read_to_string("inputs/09.txt").unwrap();
    c.bench_function("test d09p1", |b| b.iter(|| day_09::part1(&input)));
    c.bench_function("test d09p2", |b| b.iter(|| day_09::part2(&input)));
}
pub fn day_11(c: &mut Criterion) {
    let input = std::fs::read_to_string("inputs/11.txt").unwrap();
    c.bench_function("test d11p1", |b| b.iter(|| day_11::part1(&input)));
    c.bench_function("test d11p2", |b| b.iter(|| day_11::part2(&input)));
}
pub fn day_12(c: &mut Criterion) {
    let input = std::fs::read_to_string("inputs/12.txt").unwrap();
    c.bench_function("test d12p1", |b| b.iter(|| day_12::part1(&input)));
    c.bench_function("test d12p2", |b| b.iter(|| day_12::part2(&input)));
}
pub fn day_16(c: &mut Criterion) {
    let input = std::fs::read_to_string("inputs/16.txt").unwrap();
    c.bench_function("test d16p1", |b| b.iter(|| day_16::part1(&input)));
    c.bench_function("test d16p2", |b| b.iter(|| day_16::part2(&input)));
}
pub fn day_20(c: &mut Criterion) {
    let input = std::fs::read_to_string("inputs/20.txt").unwrap();
    c.bench_function("test d20p1", |b| b.iter(|| day_20::part1(&input)));
    c.bench_function("test d20p2", |b| b.iter(|| day_20::part2(&input)));
}

criterion_group!(benches, day_06, day_09, day_11, day_12, day_16, day_20);
criterion_main!(benches);