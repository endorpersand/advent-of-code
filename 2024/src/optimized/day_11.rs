use rustc_hash::FxHashMap;

fn parse(input: &str) -> Vec<usize> {
    input.split_whitespace()
        .map(str::parse)
        .collect::<Result<_, _>>()
        .unwrap()
}
fn exec(input: &[usize], iterations: usize) -> usize {
    let mut ctr: FxHashMap<usize, usize> = input.iter()
        .map(|&s| (s, 1))
        .collect();

    fn insert(m: &mut FxHashMap<usize, usize>, keys: &[usize], n: usize) {
        keys.iter().copied().for_each(|k| *m.entry(k).or_default() += n);
    }
    for _ in 0..iterations {
        for (n, c) in std::mem::take(&mut ctr) {
            match n {
                0 => insert(&mut ctr, &[1], c),
                n if n.ilog10() % 2 == 1 => {
                    let e = (n.ilog10() + 1) / 2;
                    insert(&mut ctr, &[n / 10usize.pow(e), n % 10usize.pow(e)], c)
                }
                n => insert(&mut ctr, &[n * 2024], c)
            }
        }
    }

    ctr.into_values().sum()
}

pub fn part1(input: &str) -> usize {
    let data = parse(input);
    exec(&data, 25)
}
pub fn part2(input: &str) -> usize {
    let data = parse(input);
    exec(&data, 75)
}

#[cfg(test)]
mod test {
    #[test]
    fn d11_correct() {
        let input = std::fs::read_to_string("inputs/11.txt").unwrap();
        assert_eq!(super::part1(&input), 217812);
        assert_eq!(super::part2(&input), 259112729857522);
    }
}
