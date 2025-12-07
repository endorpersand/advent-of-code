fn parse(input: &str) -> Vec<Vec<bool>> {
    input.lines()
        .map(|s| {
            s.bytes()
                .map(|b| b != b'.')
                .collect()
        }).collect()
}
pub fn part1(input: &str) -> usize {
    let beams = parse(input);
    let (first, rest) = beams.split_first().unwrap();
    
    let mut beams = first.clone();
    let mut splits = 0;
    for l in rest {
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
    let beams = parse(input);
    let (first, rest) = beams.split_first().unwrap();
    
    let mut beams: Vec<_> = first.iter()
        .copied()
        .map(usize::from)
        .collect();
    for l in rest {
        for (i, &split) in l.iter().enumerate() {
            if split && beams[i] > 0 {
                beams[i - 1] += beams[i];
                beams[i + 1] += beams[i];
                beams[i] = 0;
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
