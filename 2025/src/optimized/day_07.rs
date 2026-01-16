#[inline]
fn parse(input: &str) -> (&[u8], usize) {
    let len = input.find('\n').unwrap() + 1;
    (input.as_bytes(), len)
}
#[inline]
fn is_hit(b: u8) -> bool {
    b & 0x40 != 0
}
pub fn part1(input: &str) -> usize {
    let (grid, len) = parse(input);
    let mut rows = grid.chunks(len);

    let mut beams: Vec<_> = rows.next().unwrap().iter()
        .map(|&b| is_hit(b))
        .collect();
    let mut splits = 0;

    for l in rows {
        for (i, &b) in l.iter().enumerate() {
            if is_hit(b) {
                unsafe {
                    splits += usize::from(*beams.get_unchecked(i));
                    let replacement = [*beams.get_unchecked(i - 1) | *beams.get_unchecked(i), false, *beams.get_unchecked(i + 1) | *beams.get_unchecked(i)];

                    std::ptr::copy_nonoverlapping(
                        replacement.as_ptr(),
                        beams.as_mut_ptr().add(i - 1),
                        3
                    );
                }
            }
        }
    }
    splits
}
pub fn part2(input: &str) -> usize {
    let (grid, len) = parse(input);
    let mut rows = grid.chunks(len);

    let mut beams: Vec<_> = rows.next().unwrap().iter()
        .map(|&b| is_hit(b))
        .map(usize::from)
        .collect();
    for l in rows {
        for (i, &b) in l.iter().enumerate() {
            if is_hit(b) {
                unsafe {
                    let replacement = [*beams.get_unchecked(i - 1) + *beams.get_unchecked(i), 0, *beams.get_unchecked(i + 1) + *beams.get_unchecked(i)];
                    std::ptr::copy_nonoverlapping(
                        replacement.as_ptr(),
                        beams.as_mut_ptr().add(i - 1),
                        3
                    );
                }
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
