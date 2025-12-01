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
    fn rotate(n: i32, r: Rotation) -> (i32, u32) {
        let nn = n + r.delta();
        
        // Count number of multiples of 100 in shift
        let d = nn.unsigned_abs() / 100 + u32::from(n > 0 && nn <= 0);
        // Result
        let m = nn.rem_euclid(100);
        (m, d)
    }

    let mut index = 50;
    let mut n_zeroes = 0;
    for &r in &rotations {
        let (new_index, extra_zeroes) = rotate(index, r);
        index = new_index;
        n_zeroes += extra_zeroes;
    }
    println!("{n_zeroes}");
}