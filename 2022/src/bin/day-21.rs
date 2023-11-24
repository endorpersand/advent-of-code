use std::collections::HashMap;
use std::fs;
use std::str::FromStr;

fn main() {
    let input = fs::read_to_string("inputs/21.txt").unwrap();
    
    let mut yells: Yells = input.parse().unwrap();
    println!("{:?}", yells.get("root"));

    let humn = input.parse::<Yells>()
        .unwrap()
        .pt2();
    println!("{:?}", humn);
}

#[derive(Debug)]
enum Operation { Plus, Minus, Mul, Div }

#[derive(Debug)]
struct PendingCalc {
    left: String,
    right: String,
    op: Operation
}

impl PendingCalc {
    fn try_apply(&self, resolved: &HashMap<String, isize>) -> Option<isize> {
        let &left = resolved.get(&self.left)?;
        let &right = resolved.get(&self.right)?;

        match self.op {
            Operation::Plus  => Some(left + right),
            Operation::Minus => Some(left - right),
            Operation::Mul   => Some(left * right),
            Operation::Div   => Some(left / right),
        }
    }
}

#[derive(Debug)]
struct Yells {
    resolved: HashMap<String, isize>,
    awaiting: HashMap<String, PendingCalc>
}

impl Yells {
    fn new() -> Self {
        Self {
            resolved: HashMap::new(),
            awaiting: HashMap::new()
        }
    }

    fn resolve_awaits(&mut self) {
        self.awaiting.retain(|k, v| {
            v.try_apply(&self.resolved).map(|i| {
                self.resolved.insert(k.clone(), i);
            }).is_none()
        });
    }

    fn get(&mut self, k: &str) -> Option<isize> {
        while !self.resolved.contains_key(k) && !self.awaiting.is_empty() {
            self.resolve_awaits();
        }
        self.resolved.get(k).copied()
    }

    fn pt2(mut self) -> isize {
        self.resolved.remove("humn");
        let root = self.awaiting.remove("root").unwrap();

        let mut awaiting = self.awaiting.len();
        loop {
            self.resolve_awaits();
            if awaiting != self.awaiting.len() {
                awaiting = self.awaiting.len();
            } else {
                break;
            }
        }
        
        let resolved = self.resolved;
        let mut awaiting = self.awaiting;
        let (r0, r1) = (root.left, root.right);

        // create equation:
        // pcs are the operations applied to get from humn -> root
        // these are right to left, so imagine folding from the right to get from humn to root
        // it's awful i know but simplifies some code
        
        // rhs is the value needed for root
        let mut pcs = vec![];

        let (mut lhs, rhs) = if let Some(&lval) = resolved.get(&r0) {
            (r1, lval)
        } else if let Some(&rval) = resolved.get(&r1) {
            (r0, rval)
        } else {
            panic!("Both sides of root are unresolved {:?} :(", (r0, r1))
        };

        let mut pc;
        while &lhs != "humn" {
            let calc = awaiting.remove(&lhs).unwrap();
            (pc, lhs) = PartialCalc::resolve(calc, &resolved);
            pcs.push(pc);
        }

        pcs.into_iter()
            .fold(rhs, |rhs, op| op.inverse(rhs))
    }
}

impl FromStr for Yells {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut y = Yells::new();
        for line in s.lines() {
            let (name, data) = line.split_once(": ").ok_or(())?;
            if let Ok(calc) = data.parse() {
                y.awaiting.insert(String::from(name), calc);
            } else if let Ok(num) = data.parse() {
                y.resolved.insert(String::from(name), num);
            } else {
                Err(())?
            }
        }

        Ok(y)
    }
}
impl FromStr for PendingCalc {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut it = s.split(' ');
        let sleft = it.next().ok_or(())?;
        let sop = it.next().ok_or(())?;
        let sright = it.next().ok_or(())?;

        let op = match sop {
            "+" => Operation::Plus,
            "-" => Operation::Minus,
            "*" => Operation::Mul,
            "/" => Operation::Div,
            _ => unimplemented!()
        };

        Ok(Self {
            left: String::from(sleft),
            right: String::from(sright),
            op,
        })
    }
}

#[derive(Debug)]
enum PartialCalc {
    Left(isize, Operation),
    Right(Operation, isize)
}
impl PartialCalc {
    fn resolve(calc: PendingCalc, resolved: &HashMap<String, isize>) -> (Self, String) {
        let PendingCalc { left, right, op } = calc;

        if let Some(&lval) = resolved.get(&left) {
            (PartialCalc::Left(lval, op), right)
        } else if let Some(&rval) = resolved.get(&right) {
            (PartialCalc::Right(op, rval), left)
        } else {
            panic!("Both sides are unresolved ({left:?} {op:?} {right:?}) :(")
        }
    }

    fn inverse(&self, rhs: isize) -> isize {
        match self {
            PartialCalc::Left(a, op) => match op {
                Operation::Plus => rhs - a,
                Operation::Minus => a - rhs,
                Operation::Mul => rhs / a,
                Operation::Div => a / rhs,
            },
            PartialCalc::Right(op, b) => match op {
                Operation::Plus => rhs - b,
                Operation::Minus => rhs + b,
                Operation::Mul => rhs / b,
                Operation::Div => rhs * b,
            },
        }
    }
}