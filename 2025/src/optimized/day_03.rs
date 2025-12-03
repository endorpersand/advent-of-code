fn max_joltage(values: &[usize], n: usize) -> usize {
    let mut result = 0;
    let mut idx = 0;

    for i in 0..n {
        let (max_val, new_idx) = values[idx..(values.len() - (n - i - 1))]
            .iter()
            .copied()
            .zip(idx..)
            .max_by_key(|&(v, i)| (v, !i))
            .unwrap();

        idx = new_idx + 1;
        result = result * 10 + max_val;
    }
    
    result
}

fn parse(input: &str) -> impl Iterator<Item=Vec<usize>> {
    input.lines()
        .map(|s| s.bytes().map(|c| usize::from(c - b'0')).collect())
}
pub fn part1(input: &str) -> usize {
    parse(input)
        .map(|b| max_joltage(&b, 2))
        .sum()
}
pub fn part2(input: &str) -> usize {
    parse(input)
        .map(|b| max_joltage(&b, 12))
        .sum()
}
#[cfg(test)]
mod test {
    #[test]
    fn day03_correct() {
        let input = std::fs::read_to_string("inputs/03.txt").unwrap();
        assert_eq!(super::part1(&input), 17321);
        assert_eq!(super::part2(&input), 171989894144198);
    }
}