use std::collections::HashMap;
use std::ops::RangeInclusive;

fn main() {
    let txt = std::fs::read_to_string("inputs/19.txt").unwrap();
    let state = parse(&txt);
    println!("{}", acceptance_score(&state));
    println!("{}", count_accept_possible(&state.rules));
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Cat {
    X = 0,
    M = 1,
    A = 2,
    S = 3,
}
impl Cat {
    fn from_u8(u: u8) -> Self {
        match u {
            b'x' => Cat::X,
            b'm' => Cat::M,
            b'a' => Cat::A,
            b's' => Cat::S,
            _ => unreachable!()
        }
    }
}
#[derive(Debug, Clone, Copy)]
enum Cond<'s> {
    Lt(Cat, usize, &'s str),
    Gt(Cat, usize, &'s str),
    Else(&'s str)
}
impl<'s> Cond<'s> {
    fn parse(s: &'s str) -> Self {
        let bytes = s.as_bytes();
        match bytes.get(1) {
            Some(&b @ (b'<' | b'>')) => {
                let cat = Cat::from_u8(bytes[0]);
                let (cmp_val_str, next) = s[2..].split_once(':').unwrap();
                let cmp_val = cmp_val_str.parse().unwrap();
                
                if b == b'<' {
                    Cond::Lt(cat, cmp_val, next)
                } else {
                    Cond::Gt(cat, cmp_val, next)
                }
            },
            _ => Cond::Else(s)
        }
    }

    fn check(&self, values: &InitValues) -> Option<&str> {
        match self {
            Cond::Lt(cat, other, s) => (values[*cat as usize] < *other).then_some(s),
            Cond::Gt(cat, other, s) => (values[*cat as usize] > *other).then_some(s),
            Cond::Else(s) => Some(s),
        }
    }
}
#[derive(Debug)]
struct Rule<'s> {
    conditions: Vec<Cond<'s>>
}
impl Rule<'_> {
    fn check(&self, values: &InitValues) -> &str {
        self.conditions.iter().find_map(|c| c.check(values))
            .expect("else")
    }
}
type InitValues = [usize; 4];
#[derive(Debug)]

struct State<'s> {
    rules: HashMap<&'s str, Rule<'s>>,
    values: Vec<InitValues>
}
fn parse(file: &str) -> State {
    let mut lines = file.lines();

    let rules = lines.by_ref()
        .take_while(|s| !s.is_empty())
        .map(|line| {
            let (name, rest) = line.split_once('{').unwrap();
            let conditions = rest[..(rest.len() - 1)].split(',')
                .map(Cond::parse)
                .collect();

            (name, Rule { conditions })
        })
        .collect();

    let values = lines.map(|line| {
            let values = line[1..(line.len() - 1)]
                .split(',')
                .map(|expr| expr.split_once('=').unwrap().1)
                .map(|s| s.parse().unwrap())
                .collect::<Box<[_]>>();

            *<Box<[_; 4]>>::try_from(values).unwrap()
        })
        .collect();

    State { rules, values }
}

fn acceptance_score(state: &State) -> usize {
    state.values.iter()
        .filter(|val| {
            let mut cond = "in";
            loop {
                match cond {
                    "A" => break true,
                    "R" => break false,
                    _ => { cond = state.rules[cond].check(val); }
                }
            }
        })
        .flatten()
        .sum()
}


type ValueRanges = [RangeInclusive<usize>; 4];
fn split(ranges: ValueRanges, c: Cond) -> (Option<ValueRanges>, Option<ValueRanges>) {
    /// Splits range at index `i`, moving `i` into the LHS range, and `i + 1` into the RHS
    fn split_range(r: RangeInclusive<usize>, i: usize) -> (Option<RangeInclusive<usize>>, Option<RangeInclusive<usize>>) {
        let (&start, &end) = (r.start(), r.end());
        if i <= start {
            (None, Some(r))
        } else if i >= end {
            (Some(r), None)
        } else {
            (Some(start..=i), Some((i + 1)..=end))
        }
    }
    fn set(mut ranges: ValueRanges, cat: Cat, r: RangeInclusive<usize>) -> ValueRanges {
        ranges[cat as usize] = r;
        ranges
    }
    match c {
        Cond::Lt(cat, val, _) => {
            let (mlr, mrr) = split_range(ranges[cat as usize].clone(), val.saturating_sub(1));
            let lhs = mlr.map(|r| set(ranges.clone(), cat, r));
            let rhs = mrr.map(|r| set(ranges.clone(), cat, r));

            (lhs, rhs)
        },
        Cond::Gt(cat, val, _) => {
            let (mrr, mlr) = split_range(ranges[cat as usize].clone(), val);
            let rhs = mrr.map(|r| set(ranges.clone(), cat, r));
            let lhs = mlr.map(|r| set(ranges.clone(), cat, r));

            (lhs, rhs)
            
        },
        Cond::Else(_) => (Some(ranges), None),
    }
}
fn count_accept_possible(rules: &HashMap<&str, Rule>) -> usize {
    let mut options = vec![("in", std::array::from_fn(|_| 1..=4000))];
    let mut accepted = vec![];

    while let Some((rule_name, mut range)) = options.pop() {
        for &c in rules[rule_name].conditions.iter() {
            let (mmatch_, mother) = split(range, c);
            let next_rule = match c {
                Cond::Lt(_, _, next_rule) => next_rule,
                Cond::Gt(_, _, next_rule) => next_rule,
                Cond::Else(next_rule) => next_rule,
            };
            if let Some(match_) = mmatch_ {
                match next_rule {
                    "A" => accepted.push(match_),
                    "R" => {},
                    _ => options.push((next_rule, match_))
                }
            }
            match mother {
                Some(other) => range = other,
                None => break,
            }
        }
    }

    accepted.into_iter()
        .map(|range| range.into_iter().map(|t| t.count()).product::<usize>())
        .sum()
}