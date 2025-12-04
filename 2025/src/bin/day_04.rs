use std::collections::HashSet;

fn main() {
    let input = std::fs::read_to_string("inputs/04.txt").unwrap();
    soln(&input);
}

type Coord = (usize, usize);
fn neighbors((r, c): Coord) -> [Coord; 8] {
    [
        (r.wrapping_add_signed(-1), c.wrapping_add_signed(-1)),
        (r.wrapping_add_signed(-1), c.wrapping_add_signed(0)),
        (r.wrapping_add_signed(-1), c.wrapping_add_signed(1)),
        (r.wrapping_add_signed(0), c.wrapping_add_signed(-1)),
        (r.wrapping_add_signed(0), c.wrapping_add_signed(1)),
        (r.wrapping_add_signed(1), c.wrapping_add_signed(-1)),
        (r.wrapping_add_signed(1), c.wrapping_add_signed(0)),
        (r.wrapping_add_signed(1), c.wrapping_add_signed(1)),
    ]
}
fn parse(input: &str) -> HashSet<Coord> {
    input.lines()
        .enumerate()
        .flat_map(|(r, l)| {
            l.bytes()
                .enumerate()
                .filter(|&(_, i)| i == b'@')
                .map(move |(c, _)| (r, c))
            })
        .collect()
}
fn soln(input: &str) {
    let grid = parse(input);

    // part 1
    let p1 = grid.iter()
        .filter(|&&c| {
            let ncount = neighbors(c).into_iter()
                .filter(|n| grid.contains(n))
                .count();

            ncount < 4
        })
        .count();
    println!("{p1}");

    // part 2
    let mut p2 = 0;
    let mut current = grid.clone();
    loop {
        let removed: HashSet<_> = current.iter()
            .filter(|&&c| {
                let ncount = neighbors(c).into_iter()
                    .filter(|n| current.contains(n))
                    .count();

                ncount < 4
            })
                .copied()
                .collect();
        
        if removed.is_empty() { break; }
        p2 += removed.len();

        for r in removed {
            current.remove(&r);
        }
    }
    println!("{p2}");
}