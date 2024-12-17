fn main() {
    let input = std::fs::read_to_string("inputs/17.txt").unwrap();
    part1(&input);
    part2(&input);
}

#[derive(Default)]
struct Machine {
    registers: [usize; 3],
    instructions: Vec<u8>,
    pc: usize,
    output: Vec<u8>
}
impl std::fmt::Display for Machine {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        const INSTRS: [&str; 8] = ["adv", "bxl", "bst", "jnz", "bxc", "out", "bdv", "cdv"];
        const COMBOS: [&str; 8] = ["0", "1", "2", "3", "A", "B", "C", "?"];

        writeln!(f, "Reg A: {}", self.registers[0])?;
        writeln!(f, "Reg B: {}", self.registers[1])?;
        writeln!(f, "Reg C: {}", self.registers[2])?;
        writeln!(f, "PC: {}", self.pc)?;
        writeln!(f)?;
        self.instructions.chunks_exact(2)
            .try_for_each(|chunk| {
                let &[ins, op] = chunk else { unreachable!() };
                write!(f, "{} ", INSTRS[usize::from(ins)])?;
                match [1, 3].contains(&ins) {
                    true => writeln!(f, "{op}"),
                    false => writeln!(f, "{}", COMBOS[usize::from(op)])
                }
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
            0 => self.registers[0] >>= self.get_op(op),

            // The bxl instruction (opcode 1) calculates the bitwise XOR of register B and the instruction's literal operand, then stores the result in register B.
            1 => self.registers[1] ^= usize::from(op),

            // The bst instruction (opcode 2) calculates the value of its combo operand modulo 8 (thereby keeping only its lowest 3 bits), then writes that value to the B register.
            2 => self.registers[1] = self.get_op(op) % 8,

            // The jnz instruction (opcode 3) does nothing if the A register is 0. However, if the A register is not zero, it jumps by setting the instruction pointer to the value of its literal operand; if this instruction jumps, the instruction pointer is not increased by 2 after this instruction.
            3 => if self.registers[0] != 0 {
                self.pc = usize::from(op);
            },

            // The bxc instruction (opcode 4) calculates the bitwise XOR of register B and register C, then stores the result in register B. (For legacy reasons, this instruction reads an operand but ignores it.)
            4 => self.registers[1] ^= self.registers[2],

            // The out instruction (opcode 5) calculates the value of its combo operand modulo 8, then outputs that value. (If a program outputs multiple values, they are separated by commas.)
            5 => self.output.push((self.get_op(op) % 8) as u8),

            // The bdv instruction (opcode 6) works exactly like the adv instruction except that the result is stored in the B register. (The numerator is still read from the A register.)
            6 => self.registers[1] = self.registers[0] >> self.get_op(op),

            // The cdv instruction (opcode 7) works exactly like the adv instruction except that the result is stored in the C register. (The numerator is still read from the A register.)
            7 => self.registers[2] = self.registers[0] >> self.get_op(op),

            _ => unreachable!()
        };
    }

    fn next(&mut self) {
        let [instr, op] = self.instructions[self.pc ..= self.pc+1] else { unreachable!() };
        self.pc += 2;
        self.run_instr(instr, op);
    }
    fn reset(&mut self) {
        let m = std::mem::take(self);
        self.instructions = m.instructions;
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
        ..Default::default()
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
    // out B % 8;
    // jnz;

    // B = A % 8;
    // B ^= 2;
    // B ^= A >> B;
    // A = A >> 3;
    // B ^= 7;
    // out B % 8;
    // jnz;
    
    let x0 = usize::from(machine.instructions[3]);
    let x1 = usize::from(machine.instructions[11]);

    let mut accum = vec![];
    let mut frontier = vec![];
    let context_switch = |accum: &mut Vec<_>, frontier: &mut Vec<_>| {
        // We hit a dead end, so return to one of the frontier options
        let Some((oi, on)) = frontier.pop() else { panic!("no solution found") };

        accum.truncate(oi);
        accum.push(on as u8);
    };
    loop {
        if machine.instructions.len() == accum.len() {
            machine.reset();
            machine.registers[0] = fold(&accum);
            machine.run();

            match machine.output == machine.instructions {
                true => break,
                false => context_switch(&mut accum, &mut frontier)
            }
            continue;
        }

        let n = machine.instructions[machine.instructions.len() - 1 - accum.len()];
        let folded = fold(&accum);
        let mut options = (0..8)
            .filter(|i| (i ^ x0 ^ x1 ^ ((folded << 3 | i) >> (i ^ x0))) % 8 == usize::from(n));

        match options.next() {
            Some(nn) => {
                // Push one of the options, and the rest to frontier (for later)
                accum.push(nn as u8);
                frontier.extend(options.map(|o| (accum.len() - 1, o)));
            },
            None => context_switch(&mut accum, &mut frontier),
        }
    }
    
    println!("{}", fold(&accum));
}