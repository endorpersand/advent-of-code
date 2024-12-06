use std::collections::HashSet;

type Position = (usize, usize);
type Orientation = (isize, isize);
type State = (Position, Orientation);

const UP: Orientation = (-1, 0);
fn rotate((dr, dc): Orientation) -> Orientation {
    (dc, -dr)
}

struct Grid {
    inner: Vec<Vec<bool>>
}
impl Grid {
    fn path_iter(&self, start: State) -> impl Iterator<Item = State> + '_ {
        std::iter::successors(Some(start), |&((r, c), (dr, dc))| {
            let (nr, nc) = r.checked_add_signed(dr).zip(c.checked_add_signed(dc))?;
            let blocked = self.inner.get(nr).and_then(|r| r.get(nc))?;
        
            match blocked {
                true  => Some(((r, c), rotate((dr, dc)))),
                false => Some(((nr, nc), (dr, dc))),
            }
        })
    }

    fn path_loops(&self, start: State) -> bool {
        let mut frontier = HashSet::new();
        self.path_iter(start).any(|st| !frontier.insert(st))
    }
}

struct Data {
    grid: Grid,
    start: State
}
fn parse(input: &str) -> Data {
    let grid = input.lines()
        .map(|s| s.bytes().map(|b| b == b'#').collect())
        .collect();
    let start = input.lines()
        .enumerate()
        .find_map(|(i, line)| {
            let (j, _) = line.bytes().enumerate().find(|&(_, b)| b == b'^')?;
            Some(((i, j), UP))
        }).unwrap();

    Data { grid: Grid { inner: grid }, start }
}
pub fn d6p1(input: &str) -> usize {
    let Data { grid, start } = parse(input);
    
    let visited: HashSet<_> = grid.path_iter(start)
        .map(|(p, _)| p)
        .collect();
    
    visited.len()
}
pub fn d6p2(input: &str) -> usize {
    let Data { mut grid, start } = parse(input);
    
    let visited: HashSet<_> = grid.path_iter(start)
        .map(|(p, _)| p)
        .collect();

    visited.into_iter()
        .filter(|&(r, c)| {
            // Filter to only the tiles that would lead to a loop
            grid.inner[r][c] = true;
            let result = grid.path_loops(start);
            grid.inner[r][c] = false;
            result
        })
        .count()
}

#[cfg(test)]
mod test {
    use crate::{d6p1, d6p2};

    #[test]
    fn d6_correct() {
        let input = std::fs::read_to_string("inputs/06.txt").unwrap();
        assert_eq!(d6p1(&input), 4454);
        assert_eq!(d6p2(&input), 1503);
    }
}
