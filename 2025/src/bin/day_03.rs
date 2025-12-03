fn main() {
    let input = std::fs::read_to_string("inputs/03.txt").unwrap();
    soln(&input);
}

struct Bank {
    values: Vec<usize>
}
impl Bank {
    fn new(n: impl IntoIterator<Item=usize>) -> Self {
        Bank { values: n.into_iter().collect() }
    }

    #[expect(unused)]
    fn max_joltage(&self, n: usize) -> usize {
        // memo[i - 1][j]: the max joltage for i batteries, from self.values[j..]
        let mut memo = vec![vec![0; self.values.len()]; n];

        // memo[1 - 1][j]: base case, max(self.values[j..])
        #[expect(clippy::needless_range_loop)]
        for j in 0..self.values.len() {
            memo[0][j] = *self.values[j..].iter().max().unwrap();
        }
        
        // memo[i - 1][j] = max(values[j] concat memo[i - 2][j + 1], memo[i - 1][j + 1])
        //  i.e., select this digit as next, or select some future digit as next
        for i in 1..n {
            for j in (0..(self.values.len() - i)).rev() {
                memo[i][j] = std::cmp::max(
                    10usize.pow(i as u32) * self.values[j] + memo[i - 1][j + 1],
                    memo[i][j + 1]
                );
            }
        }
        
        memo[n - 1][0]
    }
    fn max_joltage_greedy(&self, n: usize) -> usize {
        let mut result = 0;
        let mut idx = 0;

        for i in 0..n {
            let (offset, &max_val) = self.values[idx..(self.values.len() - (n - i - 1))]
                .iter()
                .enumerate()
                .max_by_key(|&(i, &v)| (v, !i))
                .unwrap();

            idx += offset + 1;
            result = result * 10 + max_val;
        }
        
        result
    }
}
fn parse(input: &str) -> Vec<Bank> {
    input.lines()
        .map(|s| Bank::new(
            s.bytes().map(|c| usize::from(c - b'0'))
        ))
        .collect()
}
fn soln(input: &str) {
    let banks = parse(input);

    // part 1
    let p1: usize = banks.iter()
        .map(|b| b.max_joltage_greedy(2))
        .sum();
    println!("{p1}");

    // part 2
    let p2: usize = banks.iter()
        .map(|b| b.max_joltage_greedy(12))
        .sum();
    println!("{p2}");
}