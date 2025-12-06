fn main() {
    let input = std::fs::read_to_string("inputs/06.txt").unwrap();
    soln(&input);
}

enum Op { Plus, Times }
struct Expr {
    values: Vec<usize>,
    op: Op
}
impl Expr {
    fn eval(&self) -> usize {
        match self.op {
            Op::Plus => self.values.iter().sum(),
            Op::Times => self.values.iter().product(),
        }
    }
}

fn parse1(input: &str) -> Vec<Expr> {
    let mut lines = input.lines();

    let ops = lines.next_back().unwrap().split_whitespace();
    let mut value_iters: Vec<_> = lines.map(|s| s.split_whitespace()).collect();

    let mut exprs = vec![];
    for opstr in ops {
        let op = match opstr {
            "+" => Op::Plus,
            "*" => Op::Times,
            _ => unreachable!()
        };
        let values = value_iters.iter_mut()
            .map(|s| loop {
                let n = s.next().unwrap();
                if !n.is_empty() {
                    break n.parse().unwrap();
                }
            })
            .collect();

        exprs.push(Expr { values, op })
    }

    exprs
}
fn parse2(input: &str) -> Vec<Expr> {
    let mut lines: Vec<_> = input.lines()
        .map(|s| s.bytes())
        .collect();

    let mut exprs = vec![];
    let mut curr_expr_str = vec![];
    fn process(es: &[String]) -> Expr {
        let lm1 = es[0].len() - 1;
        let op = match es[0].as_bytes().last() {
            Some(b'+') => Op::Plus,
            Some(b'*') => Op::Times,
            _ => unreachable!()
        };

        let values = es.iter().rev()
            .map(|s| s[..lm1].trim().parse().unwrap())
            .collect();
        
        Expr { op, values }
    }

    while let Some(vbytes) = lines.iter_mut().map(|l| l.next()).collect() {
        let vline = String::from_utf8(vbytes).unwrap();

        // If not all spaces, add to current list
        if !vline.trim().is_empty() {
            curr_expr_str.push(vline);
        } else {
            // Otherwise, create new expr
            exprs.push(process(&curr_expr_str));
            curr_expr_str.clear();
        }
    }
    exprs.push(process(&curr_expr_str)); // Process remaining into expr

    exprs
}


fn soln(input: &str) {
    // part 1
    let p1: usize = parse1(input).iter()
        .map(|e| e.eval())
        .sum();

    println!("{p1}");

    // part 2
    let p2: usize = parse2(input).iter()
        .map(|e| e.eval())
        .sum();

    println!("{p2}");
}