use regex::Regex;

fn main() {
    let input = std::fs::read_to_string("inputs/03.txt").unwrap();
    soln(&input);
}

enum Instruction {
    Mul(usize, usize),
    Do,
    Dont
}

fn soln(input: &str) {
    let p1 = Regex::new(r"mul\((\d+),(\d+)\)").unwrap()
        .captures_iter(input)
        .map(|c| c[1].parse::<usize>().unwrap() * c[2].parse::<usize>().unwrap())
        .sum::<usize>();
    println!("{p1}");

    let (p2, _) = Regex::new(r"mul\((\d+),(\d+)\)|don't\(()\)|do\(()\)").unwrap()
        .captures_iter(input)
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
    println!("{p2}");
}