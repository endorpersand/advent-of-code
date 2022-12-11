use std::fs;

fn main() {
    let input = fs::read_to_string("inputs/11.txt").unwrap();
    let mut lines = input.lines().peekable();
    
    let mut monkeys: Vec<Monkey> = vec![];
    while lines.peek().is_some() {
        monkeys.push(lines.by_ref().collect());
        lines.next();
    }

    let mut business = MonkeyBusiness::new(monkeys.clone());
    for _ in 0..20 {
        business.perform_round1()
    }
    println!("{:?}", business.most_business());

    let mut business = MonkeyBusiness::new(monkeys);
    for _ in 0..10000 {
        business.perform_round2()
    }
    println!("{:?}", business.most_business());
}

#[derive(Debug, Clone, Copy)]
enum Operation {
    Add(usize),
    Mul(usize),
    Square
}

impl Operation {
    fn apply(&self, item: usize) -> usize {
        match self {
            Operation::Add(t) => item + t,
            Operation::Mul(t) => item * t,
            Operation::Square => item * item,
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct MonkeyData {
    worry: Operation,
    test: usize,
    pass_true: usize,
    pass_false: usize
}

#[derive(Debug, Clone)]
struct Monkey {
    data: MonkeyData,
    items: Vec<usize>,
    inspects: usize
}
impl Monkey {
    fn test(&self, item: usize) -> bool {
        item % self.data.test == 0
    }
}

#[derive(Debug, Clone)]
struct MonkeyBusiness {
    monkeys: Vec<Monkey>,
    monkulus: usize // PART B
}

impl MonkeyBusiness {
    fn new(monkeys: impl IntoIterator<Item=Monkey>) -> Self {
        let monkeys: Vec<_> = monkeys.into_iter().collect();
        let monkulus = monkeys.iter().map(|m| m.data.test).product(); // PART B
        Self { monkeys, monkulus }
    }

    fn perform_round1(&mut self) {
        let mut buffered_inspects = [vec![], vec![]];
        // can't use iterator here, because self.monkeys is borrowed more than once
        for idx in 0..self.monkeys.len() {
            let m = &mut self.monkeys[idx];
            // inspect item
            while let Some(it) = m.items.pop() {
                m.inspects += 1;
                let new_worry = m.data.worry.apply(it) / 3;
                buffered_inspects[m.test(new_worry) as usize].push(new_worry);
            }

            // push all items over
            let t = m.data.pass_true;
            let f = m.data.pass_false;
            self.monkeys[f].items.append(&mut buffered_inspects[0]);
            self.monkeys[t].items.append(&mut buffered_inspects[1]);
        }
    }

    fn most_business(&self) -> usize {
        let mut inspects: Vec<_> = self.monkeys.iter().map(|m| m.inspects).collect();
        inspects.sort_unstable();

        inspects.pop().unwrap() * inspects.pop().unwrap()
    }

    // PART B
    fn perform_round2(&mut self) {
        let mut buffered_inspects = [vec![], vec![]];
        // can't use iterator here, because self.monkeys is borrowed more than once
        for idx in 0..self.monkeys.len() {
            let m = &mut self.monkeys[idx];
            // inspect item
            while let Some(it) = m.items.pop() {
                m.inspects += 1;
                let new_worry = m.data.worry.apply(it) % self.monkulus; // this is the only change :)
                buffered_inspects[m.test(new_worry) as usize].push(new_worry);
            }

            // push all items over
            let t = m.data.pass_true;
            let f = m.data.pass_false;
            self.monkeys[f].items.append(&mut buffered_inspects[0]);
            self.monkeys[t].items.append(&mut buffered_inspects[1]);
        }
    }
}

impl<'a> FromIterator<&'a str> for Monkey {
    fn from_iter<T: IntoIterator<Item = &'a str>>(iter: T) -> Self {
        let mut it = iter.into_iter();

        debug_assert!(it.next().unwrap().starts_with("Monkey"));
        
        let items = it.next().unwrap()
            .trim()
            .strip_prefix("Starting items: ")
            .unwrap()
            .split(", ")
            .map(|s| s.parse().unwrap())
            .collect();
        
        let operation = it.next().unwrap()
            .trim()
            .strip_prefix("Operation: new = old ")
            .unwrap();

        let worry = match operation.split_at(1) {
            ("+", value) => Operation::Add(value.trim().parse::<usize>().unwrap()),
            ("*", value) => match value.trim().parse::<usize>() {
                Ok(t) => Operation::Mul(t),
                Err(_) => Operation::Square,
            },
            _ => unimplemented!("operation")
        };

        let test: usize = it.next().unwrap()
            .trim()
            .strip_prefix("Test: divisible by ")
            .unwrap()
            .parse()
            .unwrap();
        
        let pass_true: usize = it.next().unwrap()
            .trim()
            .strip_prefix("If true: throw to monkey ")
            .unwrap()
            .parse()
            .unwrap();
        let pass_false: usize = it.next().unwrap()
            .trim()
            .strip_prefix("If false: throw to monkey ")
            .unwrap()
            .parse()
            .unwrap();
        
        Self {
            data: MonkeyData {
                worry,
                test,
                pass_true,
                pass_false,
            },
            items,
            inspects: 0,
        }

    }
}