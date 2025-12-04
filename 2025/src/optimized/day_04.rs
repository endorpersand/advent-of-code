#[derive(Debug)]
struct Grid {
    buf: Vec<bool>,
    rows: usize,
    cols: usize
}
impl Grid {
    fn get(&self, (r, c): Coord) -> bool {
        (r < self.rows)
        && (c < self.cols)
        && self.buf[r * self.cols + c]
    }
    fn removables(&self) -> impl Iterator<Item=Coord> {
        self.buf.iter()
            .enumerate()
            .filter_map(|(i, &cell)| cell.then_some((i / self.cols, i % self.cols)))
            .filter(|&p| {
                let nc = neighbors(p)
                    .into_iter()
                    .filter(|&p| self.get(p))
                    .count();

                nc < 4
            })
    }
}
fn parse(input: &str) -> Grid {
    let cols = input.bytes().position(|c| c == b'\n').unwrap() + 1;
    let buf: Vec<_> = input.bytes().map(|c| c == b'@').collect();
    let rows = buf.len() / cols;
    
    Grid { buf, rows, cols }
}

type Coord = (usize, usize);
fn neighbors((r, c): Coord) -> [Coord; 8] {
    let [rn, rz, rp] = [r.wrapping_sub(1), r, r.wrapping_add(1)];
    let [cn, cz, cp] = [c.wrapping_sub(1), c, c.wrapping_add(1)];
    [
        (rn, cn), (rn, cz), (rn, cp),
        (rz, cn),           (rz, cp),
        (rp, cn), (rp, cz), (rp, cp),
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
            grid.buf[r * grid.cols + c] = false;
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