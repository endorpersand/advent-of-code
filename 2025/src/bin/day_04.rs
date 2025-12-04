use std::collections::HashSet;

fn main() {
    let input = std::fs::read_to_string("inputs/04.txt").unwrap();
    soln(&input);
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
fn removables(grid: &HashSet<Coord>) -> impl Iterator<Item=Coord> {
    grid.iter()
        .filter(|&&c| {
            let ncount = neighbors(c).into_iter().filter(|n| grid.contains(n)).count();
            ncount < 4
        })
        .copied()
}

fn soln(input: &str) {
    let grid = parse(input);

    // part 1
    let p1 = removables(&grid).count();
    println!("{p1}");

    // part 2
    let mut p2 = 0;
    let mut current = grid.clone();
    while let removed = removables(&current).collect::<HashSet<_>>() 
        && !removed.is_empty()
    {
        p2 += removed.len();
        for r in removed {
            current.remove(&r);
        }
    }
    
    println!("{p2}");
}