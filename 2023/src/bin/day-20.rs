use std::collections::{HashMap, VecDeque};

fn main() {
    let txt = std::fs::read_to_string("inputs/20.txt").unwrap();
    let mut state = State::parse(&txt);

    // PART A
    let mut lows = 0;
    let mut highs = 0;
    for _ in 0..1000 {
        let [l, h] = state.push_button();
        lows += l;
        highs += h;
    }
    println!("{}", lows * highs);

    // PART B
    println!("{}", state.part_b());
}

#[derive(Debug)]
struct State<'s> {
    pipes: HashMap<&'s str, Pipe<'s>>
}
impl<'s> State<'s> {
    fn parse(file: &'s str) -> Self {
        let mut pipes: HashMap<_, _> = file.lines()
            .map(|line| {
                let (mod_str, pipe_str) = line.split_once(" -> ").unwrap();
                let (module, name) = match mod_str.as_bytes()[0] {
                    b'&' => (Module::Conjunction { state: HashMap::default() }, &mod_str[1..]),
                    b'%' => (Module::FlipFlop { state: false }, &mod_str[1..]),
                    _ => (Module::Broadcaster, mod_str)
                };
                let dest = pipe_str.split(", ").collect();
    
                (name, Pipe { module, dest })
            })
            .collect();
    
        let mut conj_pipes: HashMap<_, _> = pipes.iter()
            .filter(|(_, Pipe { module, .. })| matches!(module, Module::Conjunction { .. }))
            .map(|(&k, _)| (k, HashMap::default()))
            .collect();
    
        for (&k, Pipe { dest, .. }) in pipes.iter() {
            for d in dest {
                if let Some(state) = conj_pipes.get_mut(d) {
                    state.insert(k, Pulse::default());
                }
            }
        }
        for (k, new_state) in conj_pipes {
            let Some(Pipe { module: Module::Conjunction { state }, .. }) = pipes.get_mut(k) else {
                unreachable!()
            };
            *state = new_state;
        }
    
        State { pipes }
    }

    fn push_button(&mut self) -> [usize; 2] {
        let mut lows = 0;
        let mut highs = 0;
    
        let mut exec_queue = VecDeque::from_iter([("broadcaster", (Pulse::Low, "button"))]);
        while let Some((pipe_name, (pulse, from))) = exec_queue.pop_front() {
            match pulse {
                Pulse::Low  => lows += 1,
                Pulse::High => highs += 1,
            }
    
            let Some(pipe) = self.pipes.get_mut(pipe_name) else { continue };
            if let Some(next_pulse) = pipe.module.pulse(pulse, from) {
                exec_queue.extend({
                    pipe.dest.iter()
                        .map(|&n| (n, (next_pulse, pipe_name)))
                });
            }
        }
    
        [lows, highs]
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
enum Pulse {
    #[default]
    Low, High
}
#[derive(Debug, Clone, PartialEq, Eq)]
enum Module<'s> {
    Broadcaster,
    FlipFlop { state: bool },
    Conjunction { state: HashMap<&'s str, Pulse> }
}
impl<'s> Module<'s> {
    fn pulse(&mut self, pulse: Pulse, from: &'s str) -> Option<Pulse> {
        match self {
            Module::Broadcaster => Some(pulse),
            Module::FlipFlop { state } => match pulse {
                Pulse::Low => {
                    let out_pulse = match state {
                        true  => Pulse::Low,
                        false => Pulse::High
                    };
                    *state = !*state;
                    Some(out_pulse)
                },
                Pulse::High => None,
            },
            Module::Conjunction { state } => {
                *state.get_mut(from).unwrap() = pulse; // not insert
                match state.values().all(|&p| p == Pulse::High) {
                    true => Some(Pulse::Low),
                    false => Some(Pulse::High)
                }
            }
        }
    }
}
#[derive(Debug, Clone, PartialEq, Eq)]
struct Pipe<'s> {
    module: Module<'s>,
    dest: Vec<&'s str>
}

// PART B
impl<'s> State<'s> {
    fn part_b(&self) -> usize {
        // The way the input is constructed:
        // There are binary counters (chains of `%`) that are connected to a `&`.
        // These `&`s are pointing towards another `&` (which points to the &child of `rx`).

        // These binary counters are set up in a way that they increment up to a value and then
        // reset to 0.

        // Thus, the binary counters cycle up to their value, and thus we find the LCM of all
        // the binary counters' values.

        // ...

        // For each broadcaster, traverse their chain.
        // The entry point always points to both &, %, so keep track of that.
        // After that, check if & is present and keep following the % chain until we get to the end.
        self.pipes["broadcaster"].dest.iter()
            .copied()
            .map(|entry| {
                let mut bit = 1;
                let mut data = 1;
                
                let &[n0, n1] = &*self.pipes[entry].dest else {
                    panic!("expected entry point {entry} to be pointing to two nodes")
                };
                let (central, mut current) = match matches!(self.pipes[n0].module, Module::Conjunction { .. }) {
                    true => (n0, Some(n1)),
                    false => (n1, Some(n0)),
                };
                while let Some(n) = current {
                    let dest = &self.pipes[n].dest;
                    data |= (dest.contains(&central) as usize) << bit;
                    current = dest.iter().copied().find(|&n| n != central);
                    bit += 1;
                }
                data
            })
            .fold(1, lcm)
    }
}

fn gcd(mut a: usize, mut b: usize) -> usize {
    while b != 0 {
        [a, b] = [b, a % b];
    }
    a
}
fn lcm(a: usize, b: usize) -> usize {
    a * b / gcd(a, b)
}