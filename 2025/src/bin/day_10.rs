use std::str::FromStr;

fn main() {
    let input = std::fs::read_to_string("inputs/10.txt").unwrap();
    println!("{}", part1(&input));
    println!("{}", part2(&input));
}

fn get(n: u16, i: usize) -> bool {
    (n >> i) & 1 != 0
}

struct Machine {
    indicators: u16,
    controls: Vec<u16>,
    joltages: Vec<usize>
}
impl FromStr for Machine {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut space_it = s.split_whitespace();
        let indicator_str = space_it.next().unwrap();
        let joltage_str = space_it.next_back().unwrap();

        let controls = space_it
            .map(|s| {
                let tup_list = s
                    .strip_prefix('(').unwrap()
                    .strip_suffix(')').unwrap();
                
                tup_list.split(',')
                    .map(|i| i.parse::<u32>().unwrap())
                    .fold(0, |acc, i| acc | (1 << i))
            })
            .collect();

        let indicators = indicator_str
            .strip_prefix('[').unwrap()
            .strip_suffix(']').unwrap()
            .bytes()
            .enumerate()
            .fold(0, |acc, (i, cv)| acc | (u16::from(cv == b'#') << i));

        let joltages = joltage_str
            .strip_prefix('{').unwrap()
            .strip_suffix('}').unwrap()
            .split(',')
            .map(|s| s.parse().unwrap())
            .collect();

        Ok(Self {
            indicators, controls, joltages
        })
    }
}

fn min_presses1(indicators: u16, controls: &[u16]) -> usize {
    let n_controls = controls.len();
    (1u16 .. (1 << n_controls))
        .filter(|&mask| {
            (0..n_controls)
                .filter(|&i| get(mask, i))
                .map(|i| controls[i])
                .fold(indicators, |acc, cv| acc ^ cv) == 0
        })
        .map(|mask| mask.count_ones())
        .min()
        .unwrap()
        as usize
}
fn parse(input: &str) -> Vec<Machine> {
    input.lines().map(|l| l.parse().unwrap()).collect()
}

fn part1(input: &str) -> usize {
    let machines = parse(input);

    machines.iter()
        .map(|m| min_presses1(m.indicators, &m.controls))
        .sum()
}

fn part2(input: &str) -> usize {
    0
}
