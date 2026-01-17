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

#[inline]
fn simd_hit(s: u8x64) -> u64 {
    s.simd_eq(u8x64::splat(b'^')).to_bitmask()
}
#[inline]
fn simd_hits(s: &[u8]) -> u64x4 {
    let (&[a, b], c) = s.as_chunks() else {
        unreachable!()
    };
    u64x4::from_array([
        simd_hit(u8x64::from_array(a)),
        simd_hit(u8x64::from_array(b)),
        simd_hit(u8x64::load_or_default(c)),
        0
    ])
}

pub fn part1(input: &str) -> usize {
    const LEN: usize = 142;
    const HALF: usize = LEN / 2 - 1;
    const OFFSET: usize = HALF - 31;
    
    let (grid, len) = parse(input);
    debug_assert_eq!(len, LEN);
    
    let mut rows = grid.chunks(len).step_by(2).skip(1);
    let mut splits = 0;

    // Beam width <= 64
    let mut beams = 1u64 << 31;

    for _ in 1..=31 { // maximum distance from center
        let splitters = unsafe {
            let l = rows.next().unwrap_unchecked().get_unchecked(OFFSET..);
            simd_hit(u8x64::load_or_default(l))
        };
        let split_beams = beams & splitters;

        splits += split_beams.count_ones();
        beams &= !split_beams;
        beams |= split_beams << 1;
        beams |= split_beams >> 1;
    }

    // Beam width > 64
    let mut beams = u64x4::from_array([
        beams << OFFSET,
        beams >> (64 - OFFSET),
        0,
        0
    ]);
    for splitters in rows.map(simd_hits) {
        let split_beams = beams & splitters;

        let o_beams = (split_beams << 1)
            | (split_beams >> 1)
            | (split_beams << 63).shift_elements_left::<1>(0)
            | (split_beams >> 63).shift_elements_right::<1>(0);

        splits += split_beams.count_ones().reduce_sum() as u32;
        beams &= !split_beams;
        beams |= o_beams;
    }

    splits as usize
}
pub fn part2(input: &str) -> usize {
    const LEN: usize = 142;
    const HALF: usize = LEN / 2 - 1;

    let (grid, len) = parse(input);
    debug_assert_eq!(len, LEN);
    
    let mut rows = grid.chunks(len).step_by(2);
    let mut beams = [0; LEN];
    let start = rows.next().unwrap().iter()
        .position(|&b| is_hit(b))
        .unwrap();
    beams[start] = 1;
    
    for (i, l) in rows.enumerate() {
        let slice = unsafe { l.get_unchecked(HALF - i .. HALF + 1 + i) };
        for (j, &b) in std::iter::zip(HALF - i.., slice).step_by(2) {
            if is_hit(b) {
                unsafe {
                    let replacement = [*beams.get_unchecked(j - 1) + *beams.get_unchecked(j), 0, *beams.get_unchecked(j + 1) + *beams.get_unchecked(j)];
                    std::ptr::copy_nonoverlapping(
                        replacement.as_ptr(),
                        beams.as_mut_ptr().add(j - 1),
                        3
                    );
                }
            }
        }
    }

    beams.into_iter().sum::<usize>()
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
