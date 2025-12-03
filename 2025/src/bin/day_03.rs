fn main() {
    let input = std::fs::read_to_string("inputs/03.txt").unwrap();
    soln(&input);
}

fn first_instance_of(values: &[usize]) -> [Option<usize>; 10] {
    let mut indexes = [None; 10];
    let mut occupied = 0;

    for (i, &value) in values.iter().enumerate() {
        if occupied >= 10 { break; }
        if indexes[value].is_none() {
            indexes[value].replace(i);
            occupied += 1;
        }
    }
    
    indexes
}
struct Bank {
    values: Vec<usize>
}
impl Bank {
    fn new(n: impl IntoIterator<Item=usize>) -> Self {
        Bank { values: n.into_iter().collect() }
    }
    fn max_joltage(&self, n: usize) -> usize {
        let instances: Vec<_> = (0..=self.values.len())
            .map(|i| first_instance_of(&self.values[i..]))
            .collect();
        let mut max = 0;
        let mut frontier = vec![(0, 0, vec![])];

        while let Some((rest_idx, last_unopt_digit, current)) = frontier.pop() {
            if current.len() == n {
                // Completed path
                let reduced = current
                    .into_iter()
                    .fold(0, |acc, cv| acc * 10 + cv);
                max = std::cmp::max(max, reduced);

                // If we're on optimal path, then there's no reason to check other branches
                while frontier.last().is_some_and(|(_, _, c)| c.len() > last_unopt_digit) {
                    frontier.pop();
                }
            } else if rest_idx == self.values.len() {
                // In this case, we definitely didn't reach optimal
            } else {
                // Incomplete path, can add another
                let old_len = frontier.len();
                frontier.extend({
                    instances[rest_idx]
                        .iter()
                        .enumerate()
                        .filter_map(|(i, &v)| Some((i, v?)))
                        .map(|(i, v)| (rest_idx + v + 1, current.len() + 1, {
                            let mut c = current.clone();
                            c.push(i);
                            c
                        }))
                });
                if frontier.len() > old_len {
                    frontier.last_mut().unwrap().1 = last_unopt_digit;
                }
            }
        }

        max
    }
}
fn parse(input: &str) -> Vec<Bank> {
    input.lines()
        .map(|s| Bank::new(
            s.chars().map(|c| usize::from((c as u8) - b'0'))
        ))
        .collect()
}
fn soln(input: &str) {
    let banks = parse(input);

    // part 1
    let p1: usize = banks.iter()
        .map(|b| b.max_joltage(2))
        .sum();
    println!("{p1}");

    // part 2
    let p2: usize = banks.iter()
        .map(|b| b.max_joltage(12))
        .sum();
    println!("{p2}");
}