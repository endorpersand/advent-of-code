type Position = (usize, usize);
type Orientation = (isize, isize);
type State = (Position, Orientation);

const UP: Orientation = (-1, 0);
fn rotate((dr, dc): Orientation) -> Orientation {
    (dc, -dr)
}
fn get_bit(o: Orientation) -> usize {
    match o {
        (-1, 0) => 0,
        (0, 1) => 1,
        (1, 0) => 2,
        (0, -1) => 3,
        _ => unreachable!()
    }
}

const N: usize = 130;
#[derive(Clone)]
struct Grid<T> {
    inner: Vec<[T; N]>
}
impl<T: Copy> Grid<T> {
    fn new(rows: usize) -> Self where T: Default {
        let inner = std::iter::repeat_with(|| std::array::from_fn(|_| Default::default()))
            .take(rows)
            .collect();

        Self { inner }
    }
    fn clear(&mut self) where T: Default {
        for n in &mut self.inner {
            n.fill_with(Default::default);
        }
    }
    fn get(&self, (r, c): Position) -> Option<&T> {
        self.inner.get(r)?.get(c)
    }
}
impl Grid<bool> {
    fn count_true(&self) -> usize {
        self.inner.iter()
            .flat_map(|row| row.iter().copied())
            .map(usize::from)
            .sum()
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

    fn path_loops(&self, start: State, frontier: &mut Grid<u8>) -> bool {
        self.path_iter(start).any(|((r, c), o)| {
            // Check if frontier has matching position and orientation,
            // and update frontier:
            let id = get_bit(o);
            let bits = frontier[r][c];
            frontier[r][c] |= 1 << id;
            bits & (1 << id) != 0
        })
    }
}
impl<T> std::ops::Index<usize> for Grid<T> {
    type Output = [T];
    
    fn index(&self, index: usize) -> &Self::Output {
        &self.inner[index]
    }
}
impl<T> std::ops::IndexMut<usize> for Grid<T> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.inner[index]
    }
}
struct Data {
    grid: Grid<bool>,
    start: State
}
fn parse(input: &str) -> Data {
    let grid = input.lines()
        .map(|s| {
            let data: Vec<bool> = s.bytes().map(|b| b == b'#').collect();
            *Box::<[bool; N]>::try_from(data).unwrap()
        })
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
    let mut visited = Grid::<bool>::new(grid.inner.len());

    for ((r, c), _) in grid.path_iter(start) {
        visited[r][c] = true;
    }
    visited.count_true()
}
pub fn d6p2(input: &str) -> usize {
    let Data { grid, start } = parse(input);
    
    let mut iter = grid.path_iter(start);
    let mut visited = Grid::<bool>::new(grid.inner.len());
    let mut current = iter.next().unwrap();
    let mut counter = 0;

    let mut grid2 = grid.clone();
    let mut frontier = Grid::<u8>::new(grid.inner.len());
    // Iterate through visited array
    // Every time the next square is empty, pretend it isn't and see what happens
    for state @ ((nr, nc), _) in iter {
        if !std::mem::replace(&mut visited.inner[nr][nc], true) && !grid[nr][nc] {
            grid2[nr][nc] = true;
            let result = grid2.path_loops(current, &mut frontier);
            frontier.clear();
            grid2[nr][nc] = false;
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
