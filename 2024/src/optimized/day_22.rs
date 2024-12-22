use rustc_hash::{FxHashMap, FxHashSet};

#[allow(clippy::let_and_return)]
fn next_secret(secret: usize) -> usize {
    let secret = (secret ^ (secret <<  6)) & 0xFFFFFF;
    let secret = (secret ^ (secret >>  5)) & 0xFFFFFF;
    let secret = (secret ^ (secret << 11)) & 0xFFFFFF;
    secret
}

fn secrets_iter(secret: usize) -> impl Iterator<Item = usize> {
    std::iter::successors(Some(secret), |&n| Some(next_secret(n)))
}
fn all_quads(seq: &[usize]) -> impl DoubleEndedIterator<Item=(u32, u32)> + use<'_> {
    seq.windows(5).map(|w| {
        let value = (w[4] % 10) as u32;
        let group = std::array::from_fn(|j| {
            let s0 = (w[j + 1] % 10) as u8;
            let s1 = (w[j] % 10) as u8;
            s0.wrapping_sub(s1)
        });
        (u32::from_ne_bytes(group), value)
    })
}

pub fn part1(input: &str) -> usize {
    let secrets: Vec<usize> = input.lines()
        .flat_map(str::parse)
        .collect();
    
    secrets.iter()
        .map(|&n| secrets_iter(n).nth(2000).unwrap())
        .sum()
}
pub fn part2(input: &str) -> usize {
    let secrets: Vec<usize> = input.lines()
    .flat_map(str::parse)
    .collect();

    let maps: Vec<FxHashMap<_, _>> = secrets.iter()
        .map(|&n| {
            let seq: Vec<_> = secrets_iter(n).take(2001).collect();
            all_quads(&seq).rev().collect()
        })
        .collect();

    let keys: FxHashSet<_> = maps.iter()
        .flat_map(|m| m.keys())
        .collect();
    keys.into_iter()
        .map(|k| {
            maps.iter()
                .map(|m| m.get(k).map_or(0, |&n| n) as usize)
                .sum()
        })
        .max()
        .unwrap()
}

#[cfg(test)]
mod test {
    #[test]
    fn d22_correct() {
        let input = std::fs::read_to_string("inputs/22.txt").unwrap();
        assert_eq!(super::part1(&input), 19150344884);
        assert_eq!(super::part2(&input), 2121);
    }
}