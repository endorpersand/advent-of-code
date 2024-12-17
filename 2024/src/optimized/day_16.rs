use std::cmp::{Ordering, Reverse};
use std::collections::BinaryHeap;

type Position = (usize, usize);
type PosDelta = (isize, isize);
type State = (Position, Direction);

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
enum Direction {
    North = 0, East = 1, South = 2, West = 3
}
impl Direction {
    fn delta(self) -> PosDelta {
        match self {
            Direction::North => (-1,  0),
            Direction::East  => ( 0,  1),
            Direction::South => ( 1,  0),
            Direction::West  => ( 0, -1),
        }
    }

    fn translate(self, (r, c): Position) -> Position {
        let (dr, dc) = self.delta();
        (r.wrapping_add_signed(dr), c.wrapping_add_signed(dc))
    }
}
impl From<u8> for Direction {
    fn from(value: u8) -> Self {
        match value % 4 {
            0 => Direction::North,
            1 => Direction::East,
            2 => Direction::South,
            3 => Direction::West,
            _ => unreachable!()
        }
    }
}

struct Grid<T> {
    grid: Vec<Vec<T>>
}
impl<T> Grid<T> {
    fn new(default: T, rows: usize, cols: usize) -> Self where T: Clone {
        Self { grid: vec![vec![default; cols]; rows] }
    }

    fn get(&self, (r, c): Position) -> Option<&T> {
        self.grid.get(r)?.get(c)
    }
}
impl Grid<bool> {
    fn neighbors(&self, (p, d): State) -> impl Iterator<Item=(usize, State)> + '_ {
        [Direction::from((d as u8).wrapping_sub(1)), d, Direction::from((d as u8).wrapping_add(1))]
            .into_iter()
            .map(move |nd| (nd.translate(p), nd)) // generate new state
            .filter(|&(np, _)| self.get(np).is_some_and(|c| !c)) // only allow moves to empty spot
            .map(move |st @ (_, nd)| (1 + usize::from(nd != d) * 1000, st)) // add cost of rotation
    }

    fn count_set(&self) -> usize {
        self.grid.iter()
            .map(|r| r.iter().filter(|&&c| c).count())
            .sum()
    }
}
impl Grid<u8> {
    fn insert(&mut self, ((r, c), d): State) -> bool {
        let is_clear = self.grid[r][c] & (1 << (d as u8)) == 0;
        self.grid[r][c] |= 1 << (d as u8);
        is_clear
    }
}
struct Data {
    grid: Grid<bool>,
    start: State,
    end: Position
}

fn parse(input: &str) -> Data {
    let mut start = None;
    let mut end = None;

    let grid = input.lines().enumerate()
        .map(|(r, l)| l.bytes().enumerate().map(|(c, b)| match b {
            b'#' => true,
            b'.' => false,
            b'S' => { start.replace((r, c)); false },
            b'E' => { end.replace((r, c)); false },
            b => unreachable!("{}", char::from(b))
        }).collect())
        .collect();

    Data { grid: Grid { grid }, start: (start.unwrap(), Direction::East), end: end.unwrap() }
}
pub fn part1(input: &str) -> usize {
    let Data { grid, start, end } = parse(input);

    let mut visited = Grid::new(0, grid.grid.len(), grid.grid[0].len());
    let mut pq = BinaryHeap::new();
    let mut result = None;

    pq.push((Reverse(0usize), start));
    while let Some((Reverse(dist), st @ (p, _))) = pq.pop() {
        if p == end {
            result.replace(dist);
            break;
        }
        if visited.insert(st) {
            pq.extend({
                grid.neighbors(st)
                    .map(|(w, nst)| (Reverse(dist + w), nst))
            });
        }
    }

    result.unwrap()
}
pub fn part2(input: &str) -> usize {
    let Data { grid, start, end } = parse(input);

    let mut dist_map = Grid::new([(usize::MAX, [None; 4]); 4], grid.grid.len(), grid.grid[0].len());
    let mut pq = BinaryHeap::new();
    let mut end_state = None;

    pq.push((Reverse(0usize), start, [None; 4]));
    while let Some((Reverse(dist), st @ (p @ (r, c), d), parents)) = pq.pop() {
        if end_state.is_some_and(|(ed, _)| dist > ed) { continue; }
        let (old_dist, old_parents) = &mut dist_map.grid[r][c][d as usize];
        match dist.cmp(old_dist) {
            Ordering::Greater => {},
            Ordering::Equal => {
                let i = old_parents.partition_point(|st| st.is_some());
                let j = parents.partition_point(|st| st.is_some());
                old_parents[i..i+j].copy_from_slice(&parents[..j]);
            },
            Ordering::Less => {
                *old_dist = dist;
                *old_parents = parents;
                match p == end {
                    true => { end_state.get_or_insert((dist, st)); },
                    false => pq.extend({
                        grid.neighbors(st)
                            .map(|(w, nst)| (Reverse(dist + w), nst, [Some(st), None, None, None]))
                    }),
                }
            },
        }
    }

    let mut visited = Grid::new(false, grid.grid.len(), grid.grid[0].len());
    if let Some((_, end_st)) = end_state {
        let mut frontier = vec![end_st];
        while let Some(node @ ((nr, nc), nd)) = frontier.pop() {
            visited.grid[nr][nc] = true;
            if node == start { continue; }
    
            let (_, parents) = dist_map.grid[nr][nc][nd as usize];
            frontier.extend(parents.iter().take_while(|s| s.is_some()).flatten());
        }
    }

    visited.count_set()
}

#[cfg(test)]
mod test {
    #[test]
    fn d16_correct() {
        let input = std::fs::read_to_string("inputs/16.txt").unwrap();
        assert_eq!(super::part1(&input), 85396);
        assert_eq!(super::part2(&input), 428);
    }
}