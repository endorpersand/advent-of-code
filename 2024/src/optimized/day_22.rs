use rustc_hash::FxHashMap;

#[allow(clippy::let_and_return)]
fn next_secret(mut secret: u32) -> u32 {
    secret ^= (secret <<  6) & 0xFFFFFF;
    secret ^= (secret >>  5) & 0xFFFFFF;
    secret ^= (secret << 11) & 0xFFFFFF;
    secret
}

fn secrets_iter(secret: u32) -> impl Iterator<Item = u32> {
    std::iter::successors(Some(secret), |&n| Some(next_secret(n)))
}
fn all_quads(seq: &[u32]) -> impl DoubleEndedIterator<Item=(u32, u32)> + use<'_> {
    seq.windows(5).map(|w| {
        let value = w[4] % 10;
        let group = std::array::from_fn(|j| {
            let s0 = (w[j + 1] % 10) as u8;
            let s1 = (w[j] % 10) as u8;
            s0.wrapping_sub(s1)
        });
        (u32::from_ne_bytes(group), value)
    })
}

fn parse(input: &str) -> Vec<u32> {
    input.lines()
        .flat_map(str::parse)
        .collect()
}
pub fn part1(input: &str) -> u64 {
    parse(input).into_iter()
        .map(|n| secrets_iter(n).nth(2000).unwrap())
        .map(u64::from)
        .sum()
}
pub fn part2(input: &str) -> u32 {
    let mut joined = FxHashMap::default();
    for n in parse(input) {
        let seq: Vec<_> = secrets_iter(n).take(2001).collect();
        let map: FxHashMap<_, _> = all_quads(&seq).rev().collect();
        for (k, v) in map {
            *joined.entry(k).or_default() += v;
        }
    }
    joined.into_values()
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