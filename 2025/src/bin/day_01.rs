fn main() {
    let input = std::fs::read_to_string("inputs/01.txt").unwrap();
    soln(&input);
}

#[derive(Clone, Copy, Debug)]
struct Rotation {
    right: bool,
    to: u32
}
impl Rotation {
    fn delta(self) -> i32 {
        match self.right {
            true  => self.to as i32,
            false => -(self.to as i32)
        }
    }
}
fn parse(input: &str) -> Vec<Rotation> {
    input.lines()
        .map(|l| {
            let right = l.as_bytes()[0] == b'R';
            let to = l[1..].parse().unwrap();
            Rotation { right, to }
        })
        .collect()
}
fn soln(input: &str) {
    let rotations = parse(input);

    // part 1
    let mut index = 50;
    let mut n_zeroes = 0;
    for r in &rotations {
        index = (index + r.delta()).rem_euclid(100);
        if index == 0 {
            n_zeroes += 1;
        }
    }
    println!("{n_zeroes}");

    // part 2
    let mut index: i32 = 50;
    let mut n_zeroes = 0;
    for r in &rotations {
        let old_n_zeroes = n_zeroes;
        let old_index = index;
        let delta = if r.right { 1 } else { -1 };
        for _ in 0..r.to {
            index = (index + delta).rem_euclid(100);
            if index == 0 {
                n_zeroes += 1;
            }
        }

        let a = n_zeroes - old_n_zeroes;
        let b = ((old_index + r.delta()).div_euclid(100)).abs();
        if a != b {
            println!("{a}, {b} - {old_index} {r:?}");
        }
    }
    println!("{n_zeroes}");
}