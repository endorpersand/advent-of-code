use std::sync::LazyLock;

use regex::Regex;

fn main() {
    let input = std::fs::read_to_string("inputs/03.txt").unwrap();
    soln2(&input);
}

#[allow(dead_code)]
fn soln1(input: &str) {
    enum Instruction {
        Mul(usize, usize),
        Do,
        Dont
    }

    static REGEX: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"mul\((\d+),(\d+)\)").unwrap());

    let x = REGEX.captures_iter(input)
        .map(|c| c[1].parse::<usize>().unwrap() * c[2].parse::<usize>().unwrap())
        .sum::<usize>();
    println!("{x}");

    static REGEX2: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"mul\((\d+),(\d+)\)|don't\(()\)|do\(()\)").unwrap());
    let instructions = REGEX2.captures_iter(input)
        .map(|c| {
            match c.get(1).zip(c.get(2)) {
                Some((x, y)) => Instruction::Mul(x.as_str().parse::<usize>().unwrap(), y.as_str().parse::<usize>().unwrap()),
                None => match c.get(3) {
                    Some(_) => Instruction::Dont,
                    None => match c.get(4) {
                        Some(_) => Instruction::Do,
                        None => unreachable!()
                    }
                },
            }
        });
    let mut reduce = 0;
    let mut state = true;
    for n in instructions {
        match n {
            Instruction::Mul(x, y) => if state { reduce += x * y },
            Instruction::Do => state = true,
            Instruction::Dont => state = false,
        }
    }
    println!("{reduce}");
}

#[allow(dead_code)]
fn soln2(input: &str) {
    enum Instruction {
        Mul(usize, usize),
        Do,
        Dont
    }

    static REGEX: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"mul\((\d+),(\d+)\)").unwrap());

    let x = REGEX.captures_iter(input)
        .map(|c| c[1].parse::<usize>().unwrap() * c[2].parse::<usize>().unwrap())
        .sum::<usize>();
    println!("{x}");

    static REGEX2: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"mul\((\d+),(\d+)\)|don't\(()\)|do\(()\)").unwrap());
    let (reduce, _) = REGEX2.captures_iter(input)
        .map(|c| match (c.get(1), c.get(2), c.get(3), c.get(4)) {
            (Some(x), Some(y), None, None) => Instruction::Mul(x.as_str().parse().unwrap(), y.as_str().parse().unwrap()),
            (None, None, Some(_), None) => Instruction::Dont,
            (None, None, None, Some(_)) => Instruction::Do,
            _ => unreachable!()
        })
        .fold((0, true), |(reduce, enabled), i| match i {
            Instruction::Mul(x, y) => (reduce + usize::from(enabled) * x * y, enabled),
            Instruction::Do => (reduce, true),
            Instruction::Dont => (reduce, false),
        });
    println!("{reduce}");
}