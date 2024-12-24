use std::collections::HashMap;

fn main() {
    let input = std::fs::read_to_string("inputs/24.txt").unwrap();
    soln(&input);
}

#[derive(Clone, Copy)]
enum Rule {
    And, Xor, Or
}
impl Rule {
    fn apply(self, a: bool, b: bool) -> bool {
        match self {
            Rule::And => a && b,
            Rule::Xor => a ^ b,
            Rule::Or  => a || b,
        }
    }
}
struct Equations<'a> {
    values: HashMap<&'a str, bool>,
    rules: Vec<(Rule, &'a str, &'a str, &'a str)>
}
fn find_value(eqs: &Equations<'_>, symbol: char) -> usize {
    let mut vals: Vec<_> = eqs.values.iter()
        .filter(|(k, _)| k.starts_with(symbol))
        .collect();
    vals.sort();
    vals.into_iter()
        .rev()
        .map(|(_, &b)| b)
        .fold(0, |acc, cv| (acc << 1) + usize::from(cv))
}

fn parse(input: &str) -> Equations<'_> {
    let mut lines = input.lines();

    let values = lines.by_ref()
        .take_while(|l| !l.is_empty())
        .map(|l| {
            let (var, n) = l.split_once(": ").unwrap();
            (var, n == "1")
        })
        .collect();
    let rules = lines.map(|l| {
        let (a, rest) = l.split_once(' ').unwrap();
        let (o, rest) = rest.split_once(' ').unwrap();
        let (b, c) = rest.split_once(" -> ").unwrap();
        let op = match o {
            "AND" => Rule::And,
            "XOR" => Rule::Xor,
            "OR" => Rule::Or,
            _ => unreachable!()
        };

        (op, a, b, c)
    })
        .collect();

    Equations { values, rules }

}
fn soln(input: &str) {
    let mut data = parse(input);

    while !data.rules.is_empty() {
        data.rules.retain(|&(r, a, b, c)| {
            let mav = data.values.get(a);
            let mbv = data.values.get(b);

            match mav.zip(mbv) {
                Some((&av, &bv)) => {
                    data.values.insert(c, r.apply(av, bv));
                    false
                },
                None => true,
            }
        });
    }

    let p1 = find_value(&data, 'z');
    println!("{p1}");
}