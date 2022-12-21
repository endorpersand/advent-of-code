use std::collections::HashMap;
use std::fs;
use std::str::FromStr;

fn main() {
    let input = fs::read_to_string("inputs/21.txt").unwrap();
    
    let mut yells: Yells = input.parse().unwrap();
    println!("{:?}", yells.get("root"));

    let mut yells = input.parse::<Yells>().unwrap().setup_pt2().into_equation();
    println!("{:?}", yells.solve());
}

#[derive(Debug)]
enum Operation { Plus, Minus, Mul, Div }

#[derive(Debug)]
struct Calculation {
    left: String,
    right: String,
    op: Operation
}

impl Calculation {
    fn try_apply(&self, y: &HashMap<String, isize>) -> Option<isize> {
        let &left = y.get(&self.left)?;
        let &right = y.get(&self.right)?;

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
    awaiting: HashMap<String, Calculation>
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
            if let Some(i) = v.try_apply(&self.resolved) {
                self.resolved.insert(k.clone(), i);
                false
            } else {
                true
            }
        });
    }

    fn get(&mut self, k: &str) -> Option<isize> {
        while !self.resolved.contains_key(k) && !self.awaiting.is_empty() {
            self.resolve_awaits();
        }
        self.resolved.get(k).copied()
    }

    fn setup_pt2(mut self) -> Yells2 {
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
        
        Yells2 {
            resolved: self.resolved,
            awaiting: self.awaiting,
            root: (root.left, root.right),
        }
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
impl FromStr for Calculation {
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
struct Yells2 {
    resolved: HashMap<String, isize>,
    awaiting: HashMap<String, Calculation>,
    root: (String, String)
}

impl Yells2 {
    fn into_equation(mut self) -> Equation {
        let mut pcs = vec![];

        let (r0, r1) = self.root.clone();
        let (mut lhs, rhs) = if let Some(&lval) = self.resolved.get(&r0) {
            (r1, lval)
        } else if let Some(&rval) = self.resolved.get(&r1) {
            (r0, rval)
        } else {
            panic!("Both sides of root are unresolved ({:?}) :(", self.root)
        };

        let mut pc;
        while &lhs != "humn" {
            let calc = self.awaiting.remove(&lhs).unwrap();
            (pc, lhs) = self.partial(calc);
            pcs.push(pc);
        }

        Equation {
            lhs: pcs.into_iter().rev().collect(),
            rhs
        }
    }

    fn partial(&self, calc: Calculation) -> (PartialCalc, String) {
        let Calculation { left, right, op } = calc;

        if let Some(&lval) = self.resolved.get(&left) {
            (PartialCalc::Left(lval, op), right)
        } else if let Some(&rval) = self.resolved.get(&right) {
            (PartialCalc::Right(op, rval), left)
        } else {
            panic!("Both sides are unresolved ({left:?} {op:?} {right:?}) :(")
        }
    }
}

#[derive(Debug)]
enum PartialCalc {
    Left(isize, Operation),
    Right(Operation, isize)
}
impl PartialCalc {
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

#[derive(Debug)]
struct Equation {
    lhs: Vec<PartialCalc>,
    rhs: isize
}
impl Equation {
    fn solve(self) -> isize {
        let Equation { lhs, rhs } = self;

        lhs.iter().rev()
            .fold(rhs, |rhs, op| op.inverse(rhs))
    }
}