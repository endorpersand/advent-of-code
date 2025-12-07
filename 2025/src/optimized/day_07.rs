fn parse(input: &str) -> (Vec<bool>, usize) {
    let len = input.find('\n').unwrap() + 1;
    let grid = input.bytes()
        .map(|b| b == b'^' || b == b'S')
        .collect();
    (grid, len)
}
pub fn part1(input: &str) -> usize {
    let (grid, len) = parse(input);
    let mut rows = grid.chunks(len);

    let mut beams = rows.next().unwrap().to_vec();
    let mut splits = 0;
    for l in rows {
        for (i, &split) in l.iter().enumerate() {
            if split && beams[i] {
                beams[i - 1 ..= i + 1].copy_from_slice(&[true, false, true]);
                splits += 1;
            }
        }
    }
    splits
}
pub fn part2(input: &str) -> usize {
    let (grid, len) = parse(input);
    let mut rows = grid.chunks(len);

    let mut beams: Vec<_> = rows.next().unwrap().iter()
        .copied()
        .map(usize::from)
        .collect();
    for l in rows {
        for (i, &split) in l.iter().enumerate() {
            if split {
                let replacement = [beams[i - 1] + beams[i], 0, beams[i + 1] + beams[i]];
                beams[i - 1 ..= i + 1].copy_from_slice(&replacement);
            }
        }
    }

    beams.iter().sum()
}


#[cfg(test)]
mod test {
    #[test]
    fn day07_correct() {
        let input = std::fs::read_to_string("inputs/07.txt").unwrap();
        assert_eq!(super::part1(&input), 1516);
        assert_eq!(super::part2(&input), 1393669447690);
    }
}
