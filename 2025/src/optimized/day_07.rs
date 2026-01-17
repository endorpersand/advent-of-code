use std::simd::prelude::*;

#[inline]
fn parse(input: &str) -> (&[u8], usize) {
    let len = input.find('\n').unwrap() + 1;
    (input.as_bytes(), len)
}
#[inline]
fn is_hit(b: u8) -> bool {
    b & 0x40 != 0
}

fn as_simd(s: &[u8]) -> [u8x64; 3] {
    let (&[a, b], c) = s.as_chunks() else {
        unreachable!()
    };
    
    [
        u8x64::from_array(a),
        u8x64::from_array(b),
        u8x64::load_or_default(c)
    ]
}
#[inline]
fn simd_hit(s: u8x64) -> u32 {
    const TEST: u8x64 = u8x64::splat(0x40);
    const ZERO: u8x64 = u8x64::splat(0);

    (s & TEST).simd_ne(ZERO)
        .to_bitmask()
        .count_ones()
}
fn simd_shk(s: [u8x64; 3]) -> [u8x64; 3] {
    let [a, b, c] = s;
    [
        a.shift_elements_left::<1>(b[0]) | a.shift_elements_right::<1>(0),
        b.shift_elements_left::<1>(c[0]) | b.shift_elements_right::<1>(a[63]),
        c.shift_elements_left::<1>(0)    | c.shift_elements_right::<1>(b[63]),
    ]
}

pub fn part1(input: &str) -> usize {
    let (grid, len) = parse(input);
    let mut rows = grid.chunks(len)
        .step_by(2)
        .map(as_simd);

    let mut beams = rows.next().unwrap();
    let mut splits = 0;

    for splitters in rows {
        let split_beams = std::array::from_fn(|i| beams[i] & splitters[i]);

        let o_beams = simd_shk(split_beams);
        for i in 0..3 {
            // Add splits
            splits += simd_hit(split_beams[i]);
            // Update beams
            beams[i] &= !split_beams[i];
            beams[i] |= o_beams[i];
        }
    }

    splits as usize
}
pub fn part2(input: &str) -> usize {
    let (grid, len) = parse(input);
    let mut rows = grid.chunks(len)
        .step_by(2);

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

    beams.iter().sum::<usize>()
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
