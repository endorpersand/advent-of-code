use std::collections::HashMap;
use std::rc::Rc;

fn main() {
    let input = std::fs::read_to_string("inputs/24.txt").unwrap();
    part1(&input);
    part2(&input);
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
fn part1(input: &str) {
    let mut data = parse(input);

    while !data.rules.is_empty() {
        data.rules.retain(|&(r, a, b, c)| {
            let Some(&av) = data.values.get(a) else { return true };
            let Some(&bv) = data.values.get(b) else { return true };

            data.values.insert(c, r.apply(av, bv));
            false
        });
    }

    let mut zbits = [false; 46]; // z00-z45
    for (k, v) in data.values {
        // Get index of k
        let Some(s) = k.strip_prefix('z') else { continue };
        let Ok(kid) = s.parse::<usize>() else { continue };
        zbits[kid] = v;
    }

    let p1 = zbits.into_iter()
        .rev()
        .fold(0, |acc, cv| (acc << 1) + usize::from(cv));
    println!("{p1}");
}

enum RuleTree<'a> {
    And(Rc<RuleTree<'a>>, Rc<RuleTree<'a>>),
    Xor(Rc<RuleTree<'a>>, Rc<RuleTree<'a>>),
    Or(Rc<RuleTree<'a>>, Rc<RuleTree<'a>>),
    Symbol(&'a str)
}
impl std::fmt::Debug for RuleTree<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::And(arg0, arg1) => write!(f, "AND({arg0:?}, {arg1:?})"),
            Self::Xor(arg0, arg1) => write!(f, "XOR({arg0:?}, {arg1:?})"),
            Self::Or(arg0, arg1)  => write!(f, "OR({arg0:?}, {arg1:?})"),
            Self::Symbol(arg0)    => write!(f, "{arg0}"),
        }
    }
}
impl RuleTree<'_> {
    fn height(&self) -> usize {
        match self {
            RuleTree::And(l, r) => l.height().max(r.height()) + 1,
            RuleTree::Xor(l, r) => l.height().max(r.height()) + 1,
            RuleTree::Or(l, r)  => l.height().max(r.height()) + 1,
            RuleTree::Symbol(_) => 0,
        }
    }
}
impl Rule {
    fn apply2<'a>(self, a: &Rc<RuleTree<'a>>, b: &Rc<RuleTree<'a>>) -> Rc<RuleTree<'a>> {
        let (a, b) = match a.height() <= b.height() {
            true  => (a.clone(), b.clone()),
            false => (b.clone(), a.clone()),
        };
        Rc::new(match self {
            Rule::And => RuleTree::And(a, b),
            Rule::Xor => RuleTree::Xor(a, b),
            Rule::Or  => RuleTree::Or(a, b),
        })
    }
}
fn swap<K: Eq + std::hash::Hash, V: Copy>(map: &mut HashMap<K, V>, k0: K, k1: K) {
    let (k0, v0) = map.remove_entry(&k0).unwrap();
    let (k1, v1) = map.remove_entry(&k1).unwrap();
    map.insert(k0, v1);
    map.insert(k1, v0);
}
fn part2(input: &str) {
    // Full-bit adder:
    // S01 = A01 ^ B01 ^ C00
    // C01 = (A01 & B01) | ((A01 ^ B01) & C00)
    let Equations { values, rules } = parse(input);
    let mut rules: HashMap<_, _> = rules.into_iter()
        .map(|(r, a, b, c)| (c, (r, a, b)))
        .collect();
    // Nah, I'm not implementing this.
    // Use the print statements on the very bottom to print out all of the trees,
    // then find the discrepancies and swap them here:
    swap(&mut rules, "z17", "cmv");
    swap(&mut rules, "z23", "rmj");
    swap(&mut rules, "z30", "rdg");
    swap(&mut rules, "mwp", "btb");

    let mut completed_rules: HashMap<_, _> = values.into_keys()
        .map(|s| (s, Rc::new(RuleTree::Symbol(s))))
        .collect();

    while !rules.is_empty() {
        rules.retain(|c, &mut (r, a, b)| {
            let Some(av) = completed_rules.get(a) else { return true };
            let Some(bv) = completed_rules.get(b) else { return true };

            let tree = r.apply2(av, bv);
            completed_rules.insert(c, tree);
            false
        });
    }

    // let mut sorted_rules: Vec<_> = completed_rules.iter().collect();
    // sorted_rules.sort_by_key(|&(k, _)| k);
    // for (sym, tree) in sorted_rules {
    //     println!("{sym}: {tree:?}");
    // }
}