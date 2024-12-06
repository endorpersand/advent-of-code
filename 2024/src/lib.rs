use rustc_hash::FxHashSet;

type Position = (usize, usize);
type Orientation = (isize, isize);
type State = (Position, Orientation);

const UP: Orientation = (-1, 0);
fn rotate((dr, dc): Orientation) -> Orientation {
    (dc, -dr)
}

#[derive(Clone)]
struct Grid {
    inner: Vec<Vec<bool>>
}
impl Grid {
    fn get(&self, (r, c): Position) -> Option<bool> {
        self.inner.get(r)?.get(c).copied()
    }
    fn path_iter(&self, start: State) -> impl Iterator<Item = State> + '_ {
        std::iter::successors(Some(start), |&((r, c), (dr, dc))| {
            let nr = r.wrapping_add_signed(dr);
            let nc = c.wrapping_add_signed(dc);
            let blocked = self.get((nr, nc))?;
        
            match blocked {
                true  => Some(((r, c), rotate((dr, dc)))),
                false => Some(((nr, nc), (dr, dc))),
            }
        })
    }

    fn path_loops(&self, start: State, frontier: &mut FxHashSet<State>) -> bool {
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

    let visited: FxHashSet<_> = grid.path_iter(start)
        .map(|(p, _)| p)
        .collect();

    visited.len()
}
pub fn d6p2(input: &str) -> usize {
    let Data { grid, start } = parse(input);
    
    let mut iter = grid.path_iter(start);
    let mut visited = FxHashSet::default();
    let mut current = iter.next().unwrap();
    let mut counter = 0;

    let mut grid2 = grid.clone();
    let mut frontier = FxHashSet::default();
    // Iterate through visited array
    // Every time the next square is empty, pretend it isn't and see what happens
    for state @ ((nr, nc), _) in iter {
        if visited.insert((nr, nc)) && !grid.inner[nr][nc] {
            grid2.inner[nr][nc] = true;
            let result = grid2.path_loops(current, &mut frontier);
            frontier.clear();
            grid2.inner[nr][nc] = false;
            if result {
                counter += 1;
            }
        }
        current = state;
    }
    
    counter
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
