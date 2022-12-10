use std::collections::VecDeque;
use std::fs;
use std::str::FromStr;

fn main() {
    let input = fs::read_to_string("inputs/10.txt").unwrap();

    let instrs: Vec<Instruction> = input.lines()
        .map(|s| s.parse().unwrap())
        .collect();
    
    let mut it = Signal::new(instrs.clone());

    // PART A
    let result: isize = (0..=5)
        .map(|idx| 20 + idx * 40)
        .flat_map(|cycle| {
            it.by_ref()
                .take_while(|&(c, _)| (c < cycle))
                .last()
                .map(|(_, s)| s * (cycle as isize))
        })
        .sum();

        
    println!("{result}");
    
    // PART B
    let mut cycle = 1;
    let mut signal: isize = 1;
    let mut it = Signal::new(instrs);
    let mut pixels: Vec<bool> = vec![];
    while let Some((after_cycle, after_value)) = it.next() {
        while cycle <= after_cycle {
            let pos: isize = ((cycle as isize) - 1) % 40;
            pixels.push((signal - 1) <= pos && pos <= (signal + 1));
            cycle += 1;
        }
        signal = after_value;
    }

    let result = pixels.into_iter()
        .map(|b| if b { '#' } else { '.' })
        .collect::<Vec<_>>()
        .chunks(40)
        .map(|chars| chars.iter().collect())
        .collect::<Vec<String>>()
        .join("\n");
    println!("{result}")

}

#[derive(Clone, Copy)]
enum Instruction {
    Add(isize),
    Noop
}
impl FromStr for Instruction {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s == "noop" {
            Ok(Instruction::Noop)
        } else {
            if let Some(int) = s.strip_prefix("addx ") {
                Ok(Instruction::Add(int.parse().unwrap()))
            } else {
                Err(())
            }
        }
    }
}

struct Signal {
    cycle: usize,
    value: isize,
    instructions: VecDeque<Instruction>
}

impl Signal {
    fn new(it: impl IntoIterator<Item=Instruction>) -> Self {
        Self { cycle: 0, value: 1, instructions: it.into_iter().collect() }
    }
}
impl Iterator for Signal {
    type Item = (usize, isize /* value after cycle completes */);

    fn next(&mut self) -> Option<Self::Item> {
        let inst = self.instructions.pop_front()?;
        match inst {
            Instruction::Add(val) => {
                self.cycle += 2;
                self.value += val;
            },
            Instruction::Noop => {
                self.cycle += 1;
            },
        }

        Some((self.cycle, self.value))
    }
}