use std::fs;
use std::str::FromStr;

fn main() {
    let input = fs::read_to_string("inputs/5.txt").unwrap();
    let mut lines = input.lines();

    // part a
    let mut stacks: Stacks = Stacks::new(lines.by_ref().take(9));
    lines.next();
    for l in lines {
        let mov = l.parse().unwrap();
        stacks.apply_move_1(mov);
    }

    let tops: String = stacks.stacks
        .iter()
        .map(|s| s.last().unwrap())
        .copied()
        .collect();
    println!("{}", tops);

    // part b
    let mut lines = input.lines();
    let mut stacks: Stacks = Stacks::new(lines.by_ref().take(9));
    lines.next();
    for l in lines {
        let mov = l.parse().unwrap();
        stacks.apply_move_2(mov);
    }

    let tops: String = stacks.stacks
        .iter()
        .map(|s| s.last().unwrap())
        .copied()
        .collect();
    println!("{}", tops)
}

fn parse_stack_line(ln: &str) -> Vec<Option<char>> {
    let mut s: String = String::from(ln);
    let mut vec = vec![];
    s.remove(0);
    while !s.is_empty() {
        let r = match s.remove(0) {
            ' ' => None,
            c => Some(c)
        };
        vec.push(r);
        s.drain(0..3.min(s.len()));
    }
    
    vec
}
#[derive(Debug)]
struct Stacks {
    stacks: Vec<Vec<char>>
}
impl Stacks {
    fn new<'a, I>(it: I) -> Self 
        where I: Iterator<Item=&'a str>
    {
        let mut vec: Vec<_> = it.collect();
        let mut stacks = vec![vec![]; 9];
        vec.pop();

        for line in vec.into_iter().rev() {
            for (i, mc) in parse_stack_line(line).into_iter().enumerate() {
                if let Some(c) = mc {stacks[i].push(c);}
            }
        }
        
        Stacks { stacks }
    }

    fn apply_move_1(&mut self, m: Move) {
        let Move { count, idx1, idx2 } = m;
        for _ in 0..count {
            let el = self.stacks[idx1].pop().unwrap();
            self.stacks[idx2].push(el);
        }
    }

    // part b
    fn apply_move_2(&mut self, m: Move) {
        let Move { count, idx1, idx2 } = m;
        let len = self.stacks[idx1].len();
        let els: Vec<_> = self.stacks[idx1].drain((len - count)..).collect();
        self.stacks[idx2].extend(els);
    }
}

#[derive(Debug)]
struct Move {
    count: usize,
    idx1: usize,
    idx2: usize
}
impl FromStr for Move {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut s = String::from(s.strip_prefix("move ").ok_or(())?);
        
        let pos = s.chars().position(|c| !c.is_numeric()).unwrap();
        let count = s.drain(0..pos).collect::<String>().parse().unwrap();
        
        s.drain(0..6);
        let idx1: usize = String::from(s.remove(0)).parse().unwrap();
        
        s.drain(0..4);
        let idx2: usize = String::from(s.remove(0)).parse().unwrap();

        Ok(Move {
            count, idx1: idx1 - 1, idx2: idx2 - 1
        })
    }
}