use std::collections::HashSet;

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

pub fn part1(input: &str) -> usize {
    let grid = parse(input);
    removables(&grid).count()
}
pub fn part2(input: &str) -> usize {
    let mut grid = parse(input);
    
    let mut p2 = 0;
    while let removed = removables(&grid).collect::<HashSet<_>>() 
        && !removed.is_empty()
    {
        p2 += removed.len();
        for r in removed {
            grid.remove(&r);
        }
    }
    p2
}
#[cfg(test)]
mod test {
    #[test]
    fn day03_correct() {
        let input = std::fs::read_to_string("inputs/04.txt").unwrap();
        assert_eq!(super::part1(&input), 1523);
        assert_eq!(super::part2(&input), 9290);
    }
}