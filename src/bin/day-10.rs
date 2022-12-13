use std::collections::VecDeque;
use std::fs;
use std::str::FromStr;

fn main() {
    let input = fs::read_to_string("inputs/10.txt").unwrap();

    let instrs: Vec<Instruction> = input.lines()
        .map(|s| s.parse().unwrap())
        .collect();
    
    let signals: Vec<_> = Signal::new(instrs).collect();

    // PART A
    let result: isize = (0..=5)
        .map(|idx| 20 + idx * 40)
        .flat_map(|cycle| signals.get(cycle - 1).map(|&signal| (cycle as isize) * signal))
        .sum();

        
    println!("{result}");
    
    // PART B

    // pixels
    let result = signals.into_iter().enumerate()
        .map(|(cyclem1, signal)| {
            let pos = (cyclem1 as isize) % 40;
            (signal - 1) <= pos && pos <= (signal + 1)
        })

        // convert to text
        .map(|b| if b { '#' } else { '.' })
        .collect::<Vec<_>>()
        .chunks(40)
        .map(String::from_iter)
        .collect::<Vec<_>>()
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
        } else if let Some(int) = s.strip_prefix("addx ") {
            Ok(Instruction::Add(int.parse().unwrap()))
        } else {
            Err(())
        }
    }
}

struct Signal {
    value: isize,
    partial: Option<Instruction>,
    instructions: VecDeque<Instruction>
}

impl Signal {
    fn new(it: impl IntoIterator<Item=Instruction>) -> Self {
        Self { value: 1, partial: None, instructions: it.into_iter().collect() }
    }
}
impl Iterator for Signal {
    type Item = isize /* during this cycle, the value is */;

    fn next(&mut self) -> Option<Self::Item> {
        match self.partial.take() {
            Some(inst) => {
                // during cycle
                let value = self.value;
                // end cycle
                if let Instruction::Add(t) = inst {
                    self.value += t;
                }

                Some(value)
            },
            None => {
                // start cycle
                let inst = self.instructions.pop_front()?;
                // during cycle
                let value = self.value;
                // end cycle
                if let Instruction::Add(_) = inst {
                    self.partial.replace(inst);
                }

                Some(value)
            },
        }
    }
}