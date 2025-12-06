fn main() {
    let input = std::fs::read_to_string("inputs/06.txt").unwrap();
    println!("{}", part1(&input));
    println!("{}", part2(&input));
}

#[derive(Clone, Copy)]
enum Op { Plus, Times }
impl Op {
    fn eval(&self, values: impl IntoIterator<Item=usize>) -> usize {
        match self {
            Op::Plus => values.into_iter().sum(),
            Op::Times => values.into_iter().product(),
        }
    }
}

fn transpose_lines<'a, I: Iterator + 'a>(input: &'a str, tokenize: impl FnMut(&'a str) -> I) -> impl Iterator<Item=Vec<I::Item>> {
    let mut token_its: Vec<_> = input.lines().map(tokenize).collect();
    std::iter::from_fn(move || token_its.iter_mut().map(|l| l.next()).collect())
}
fn part1(input: &str) -> usize {
    transpose_lines(input, str::split_whitespace)
        .map(|mut token_strs| {
            let op = match token_strs.pop() {
                Some("+") => Op::Plus,
                Some("*") => Op::Times,
                _ => unreachable!()
            };
        
            op.eval({
                token_strs.into_iter()
                    .map(|n| n.parse().unwrap())
            })
        })
        .sum()
}
fn part2(input: &str) -> usize {
    let mut sum = 0;

    let mut curr_op = Op::Plus;
    let mut curr_values = vec![];

    for mut vbytes in transpose_lines(input, str::bytes) {
        match vbytes.pop() {
            Some(b'+') => curr_op = Op::Plus,
            Some(b'*') => curr_op = Op::Times,
            _ => {}
        }

        let vline = String::from_utf8(vbytes).unwrap();
        let trimmed = vline.trim();
        // If not all spaces, add to current list
        if trimmed.is_empty() {
            sum += curr_op.eval(std::mem::take(&mut curr_values));
        } else {
            curr_values.push(trimmed.parse().unwrap());
        }
    }
    sum += curr_op.eval(curr_values);
    
    sum
}
