fn main() {
    let input = std::fs::read_to_string("inputs/17.txt").unwrap();
    // part1(&input);
    part2(&input);
}

struct Machine {
    registers: [usize; 3],
    instructions: Vec<u8>,
    pc: usize,
    output: Vec<usize>
}
impl std::fmt::Display for Machine {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Reg A: {}", self.registers[0])?;
        writeln!(f, "Reg B: {}", self.registers[1])?;
        writeln!(f, "Reg C: {}", self.registers[2])?;
        writeln!(f, "PC: {}", self.pc)?;
        writeln!(f)?;
        self.instructions.chunks_exact(2)
            .try_for_each(|chunk| {
                let &[ins, op] = chunk else { unreachable!() };
                f.write_str([
                    "adv", "bxl", "bst", "jnz", "bxc", "out", "bdv", "cdv"
                ][usize::from(ins)])?;
                f.write_str(" ")?;

                if [1, 3].contains(&ins) {
                    write!(f, "{}", chunk[1])?;
                } else {
                    f.write_str([
                        "0", "1", "2", "3", "A", "B", "C", "?"
                    ][usize::from(op)])?;
                }
                writeln!(f)
            })?;
        writeln!(f)?;
        writeln!(f, "Output: {:?}", self.output)
    }
}
impl Machine {
    fn get_op(&self, op: u8) -> usize {
        match op {
            0 => 0,
            1 => 1,
            2 => 2,
            3 => 3,
            4 => self.registers[0],
            5 => self.registers[1],
            6 => self.registers[2],
            _ => unreachable!()
        }
    }

    fn run_instr(&mut self, instr: u8, op: u8) {
        match instr {
            // The adv instruction (opcode 0) performs division. The numerator is the value in the A register. The denominator is found by raising 2 to the power of the instruction's combo operand. (So, an operand of 2 would divide A by 4 (2^2); an operand of 5 would divide A by 2^B.) The result of the division operation is truncated to an integer and then written to the A register.
            0 => {
                self.registers[0] >>= self.get_op(op);
            },
            // The bxl instruction (opcode 1) calculates the bitwise XOR of register B and the instruction's literal operand, then stores the result in register B.
            1 => {
                self.registers[1] ^= op as usize;
            },

            // The bst instruction (opcode 2) calculates the value of its combo operand modulo 8 (thereby keeping only its lowest 3 bits), then writes that value to the B register.
            2 => {
                self.registers[1] = self.get_op(op) % 8
            },

            // The jnz instruction (opcode 3) does nothing if the A register is 0. However, if the A register is not zero, it jumps by setting the instruction pointer to the value of its literal operand; if this instruction jumps, the instruction pointer is not increased by 2 after this instruction.
            3 => {
                let a = self.registers[0];
                if a != 0 {
                    self.pc = op as usize;
                }
            },

            // The bxc instruction (opcode 4) calculates the bitwise XOR of register B and register C, then stores the result in register B. (For legacy reasons, this instruction reads an operand but ignores it.)
            4 => {
                // _ = self.get_op(op);
                self.registers[1] ^= self.registers[2];
            },

            // The out instruction (opcode 5) calculates the value of its combo operand modulo 8, then outputs that value. (If a program outputs multiple values, they are separated by commas.)
            5 => {
                self.output.push(self.get_op(op) % 8);
            },

            // The bdv instruction (opcode 6) works exactly like the adv instruction except that the result is stored in the B register. (The numerator is still read from the A register.)
            6 => {
                let n = self.registers[0];
                self.registers[1] = n >> self.get_op(op);
            },

            // The cdv instruction (opcode 7) works exactly like the adv instruction except that the result is stored in the C register. (The numerator is still read from the A register.)
            7 => {
                let n = self.registers[0];
                self.registers[2] = n >> self.get_op(op);
            },

            _ => unreachable!()
        };
    }

    fn next(&mut self) {
        let [instr, op] = self.instructions[self.pc ..= self.pc+1] else { panic!() };
        self.pc += 2;
        self.run_instr(instr, op);
        // println!("{self}");
    }
    fn run(&mut self) {
        while self.pc < self.instructions.len() {
            self.next();
        }
    }
}
fn parse(input: &str) -> Machine {
    let mut lines = input.lines();
    let registers: Vec<_> = (&mut lines).take(3)
        .filter_map(|line| line.split_once(": "))
        .map(|(_, b)| b.parse().unwrap())
        .collect();
    lines.next();
    
    let prog_str = lines.next().unwrap();
    let instructions: Vec<_> = prog_str.split_once(": ")
        .unwrap().1
        .split(",")
        .map(|s| s.parse().unwrap())
        .collect();

    Machine {
        registers: registers.try_into().unwrap(),
        instructions,
        pc: 0,
        output: vec![]
    }
}
fn fold(vals: &[u8]) -> usize {
    vals.iter()
        .copied()
        .fold(0, |acc, cv| (acc << 3) | usize::from(cv))
}
fn part1(input: &str) {
    let mut machine = parse(input);
    machine.run();
    let p1 = machine.output.iter()
        .map(|s| s.to_string())
        .collect::<Vec<_>>()
        .join(",");
    println!("{p1}");
}
fn part2(input: &str) {
    let mut machine = parse(input);
    // B = A % 8;
    // B ^= 2;
    // C = A >> B;
    // B ^= C;
    // A = A >> 3;
    // B ^= 7;
    // out B;
    // jmp;


    // B = A % 8;
    // B ^= 2;
    // B ^= A >> B;
    // A = A >> 3;
    // B ^= 7;
    // out B % 8;
    // jmp;
    
    // machine.registers[0] = fold(&[2, 4, 1, 2, 7, 5, 4, 3, 0, 3, 1, 7, 5, 5, 3, 0]);
    // machine.run();
    // println!("{machine}");

    let base = machine.instructions.clone();
    let mut accum = vec![];
    let mut frontier = vec![];
    let mut i = 0;
    'outer: loop {
        for &n in base[..base.len() - i].iter().rev() {
            let folded = fold(&accum);
            let mut options = (0..8)
                .filter(|i| (i ^ 5 ^ ((folded << 3 | i) >> (i ^ 2))) % 8 == usize::from(n));

            match options.next() {
                Some(nn) => {
                    accum.push(nn as u8);
                    // println!("{i}, {accum:?}");

                    for o in options {
                        println!("option {o} > {nn}");
                        if o > nn {
                            frontier.push((i, o));
                        }
                    }
                },
                None => {
                    // panic!();
                    // No option, go back to frontier
                    println!("old: {accum:?}");
                    // println!("{frontier:?}");
                    let Some((oi, on)) = frontier.pop() else { break 'outer };
                    accum.truncate(oi);
                    accum.push(on as u8);
                    println!("new: {accum:?} ({oi}, {on})");
                    i = oi + 1;
                    continue 'outer;
                },
            }
            i += 1;
        }

        machine.registers[0] = fold(&accum);
        machine.run();
        if !machine.output.iter().copied().eq(machine.instructions.iter().copied().map(usize::from)) {
            let Some((oi, on)) = frontier.pop() else { break 'outer };
            accum.truncate(oi);
            accum.push(on as u8);
            i = oi;
            continue 'outer;
        }
        println!("{accum:?}");
        break;
    }

    machine.registers[0] = fold(&accum);
    machine.run();
    assert!(machine.output.iter().copied().eq(machine.instructions.iter().copied().map(usize::from)));
    println!("{}", fold(&accum));
    // println!("{base:?}");
}