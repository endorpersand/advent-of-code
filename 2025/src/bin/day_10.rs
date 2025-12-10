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
    let machine = parse(input);

    machine.iter()
        .map(|Machine { controls, joltages, .. }| {
            let control_vars: Vec<_> = (0..controls.len()).map(|_| z3::ast::Int::fresh_const("control")).collect();
            let optimize = z3::Optimize::new();

            for var in &control_vars {
                optimize.assert(&var.ge(0));
            }
            for (i, &jolt) in joltages.iter().enumerate() {
                let s: Vec<_> = std::iter::zip(controls, &control_vars)
                    .filter(|&(&c, _)| get(c, i))
                    .map(|(_, var)| var)
                    .collect();
                optimize.assert(&z3::ast::Int::add(&s).eq(jolt as u32));
            }

            let opt = z3::ast::Int::add(&control_vars);
            optimize.minimize(&opt);

            optimize.check(&[]);
            optimize.get_model().unwrap().eval(&opt, true).unwrap().as_u64().unwrap() as usize
        })
        .sum()
}
