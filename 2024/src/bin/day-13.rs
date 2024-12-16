fn main() {
    let input = std::fs::read_to_string("inputs/13.txt").unwrap();
    soln(&input);
}

#[derive(Debug)]
struct Machine {
    a: (isize, isize),
    b: (isize, isize),
    goal: (isize, isize)
}
impl Machine {
    fn solve(&self) -> Option<(isize, isize)> {
        let (a, c) = self.a;
        let (b, d) = self.b;
        let (g0, g1) = self.goal;
        let det = a * d - b * c;
        if det == 0 { return None; }
        let (n0, n1) = (g0 * d - g1 * b, g1 * a - g0 * c);

        (n0 % det == 0 && n1 % det == 0).then(|| (n0 / det, n1 / det))
            .filter(|&(a, b)| 0 <= a && 0 <= b)
    }
}
fn parse(input: &str) -> Option<Vec<Machine>> {
    let mut lines = input.lines();
    let mut data = vec![];

    loop {
        let a_str = lines.next()?.strip_prefix("Button A: X+")?;
        let (axs, ays) = a_str.split_once(", Y+")?;
        
        let b_str = lines.next()?.strip_prefix("Button B: X+")?;
        let (bxs, bys) = b_str.split_once(", Y+")?;

        let g_str = lines.next()?.strip_prefix("Prize: X=")?;
        let (gxs, gys) = g_str.split_once(", Y=")?;

        data.push(Machine {
            a: axs.parse().ok().zip(ays.parse().ok())?,
            b: bxs.parse().ok().zip(bys.parse().ok())?,
            goal: gxs.parse().ok().zip(gys.parse().ok())?,
        });

        if lines.next().is_none() { break; }
    }

    Some(data)
}
fn soln(input: &str) {
    let mut data = parse(input).unwrap();

    let p1: isize = data.iter()
        .filter_map(|m| m.solve())
        .filter(|&(l, r)| (l <= 100 && r <= 100))
        .map(|(l, r)| l * 3 + r)
        .sum();
    println!("{p1}");

    for m in &mut data {
        m.goal = (m.goal.0 + 10000000000000, m.goal.1 + 10000000000000);
    }
    let p2: isize = data.iter()
        .filter_map(|m| m.solve())
        .map(|(l, r)| l * 3 + r)
        .sum();
    println!("{p2}");
}