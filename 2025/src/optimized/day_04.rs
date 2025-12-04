#[derive(Debug)]
struct Grid {
    buf: Vec<Vec<bool>>,
}
impl Grid {
    fn removables(&self) -> impl Iterator<Item=Coord> {
        self.buf.iter()
            .enumerate()
            .flat_map(|(r, row)| {
                row.iter().enumerate()
                    .filter(|&(_, &cell)| cell)
                    .map(move |(c, _)| (r, c))
            })
            .filter(|&p| {
                let nc = neighbors(p)
                    .into_iter()
                    .filter(|&mn| {
                        mn
                            .and_then(|(r, c)| self.buf.get(r)?.get(c))
                            .is_some_and(|&cell| cell)
                    })
                    .count();

                nc < 4
            })
    }
}
fn parse(input: &str) -> Grid {
    let buf = input.lines()
        .map(|l| {
            l.bytes()
                .map(|c| c == b'@')
                .collect()
            })
        .collect();

    Grid { buf }
}

type Coord = (usize, usize);
fn neighbors((r, c): Coord) -> [Option<Coord>; 8] {
    let [rn, rz, rp] = [r.checked_sub(1), Some(r), r.checked_add(1)];
    let [cn, cz, cp] = [c.checked_sub(1), Some(c), c.checked_add(1)];
    [
        rn.zip(cn), rn.zip(cz), rn.zip(cp),
        rz.zip(cn),             rz.zip(cp),
        rp.zip(cn), rp.zip(cz), rp.zip(cp),
    ]
}

pub fn part1(input: &str) -> usize {
    let grid = parse(input);
    grid.removables().count()
}
pub fn part2(input: &str) -> usize {
    let mut grid = parse(input);
    
    let mut p2 = 0;
    while let removed = grid.removables().collect::<Vec<_>>() 
        && !removed.is_empty()
    {
        p2 += removed.len();
        for (r, c) in removed {
            grid.buf[r][c] = false;
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