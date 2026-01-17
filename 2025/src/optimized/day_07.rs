use std::simd::{LaneCount, SupportedLaneCount, prelude::*};

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
fn simd_hit<const N: usize>(s: Simd<u8, N>) -> u32
    where LaneCount<N>: SupportedLaneCount
{
    (s & Simd::splat(0x40))
        .simd_ne(Simd::splat(0))
        .to_bitmask()
        .count_ones()
}
fn simd_shk<const N: usize, const M: usize>(s: [Simd<u8, N>; M]) -> [Simd<u8, N>; M]
    where LaneCount<N>: SupportedLaneCount
{
    std::array::from_fn(|i| {
        let r = if i < M - 1 { s[i + 1][0] } else { 0 };
        let l = if i > 0 { s[i - 1][N - 1] } else { 0 };

        s[i].shift_elements_left::<1>(r) | s[i].shift_elements_right::<1>(l)
    })
}

pub fn part1(input: &str) -> usize {
    const LEN: usize = 142;
    const HALF: usize = LEN / 2 - 1;
    const OFFSET: usize = HALF - 32;
    
    let (grid, len) = parse(input);
    debug_assert_eq!(len, LEN);
    
    let mut rows = grid.chunks(len).step_by(2);
    let mut splits = 0;

    // Beam width <= 64
    let mut beams = u8x64::splat(b'.');
    beams[32] = b'^';

    for _ in 1..=31 { // maximum distance from center
        let splitters = unsafe {
            let l = rows.next().unwrap_unchecked();
            u8x64::load_or_default(l.get_unchecked(OFFSET..))
        };
        let split_beams = beams & splitters;

        splits += simd_hit(split_beams);
        beams &= !split_beams;
        beams |= split_beams.shift_elements_left::<1>(0);
        beams |= split_beams.shift_elements_right::<1>(0);
    }

    // Beam width > 64
    let mut beams = [
        beams.shift_elements_right::<OFFSET>(0),
        beams.shift_elements_left::<{ 64 - OFFSET }>(0),
        u8x64::splat(0)
    ];
    for l in rows {
        let splitters = as_simd(l);
        let split_beams = std::array::from_fn(|i| beams[i] & splitters[i]);

        let o_beams = simd_shk::<_, 3>(split_beams);
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
    
    let half = len / 2 - 1;
    for (i, l) in rows.enumerate() {
        let slice = unsafe { l.get_unchecked(half - i .. half + 1 + i) };
        for (j, &b) in std::iter::zip(half - i.., slice) {
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
