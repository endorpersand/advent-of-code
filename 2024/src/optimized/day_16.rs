use std::cmp::Reverse;
use std::collections::hash_map::Entry;
use std::collections::BinaryHeap;

use rustc_hash::FxHashMap;

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
        [Direction::North, Direction::East, Direction::South, Direction::West]
            .into_iter()
            .map(move |nd| (nd.translate(p), nd)) // generate new state
            .filter(|&(np, _)| self.get(np).is_some_and(|c| !c)) // only allow moves to empty spot
            .map(move |st @ (_, nd)| {
                // compute cost of rotating
                let cost = match (nd as u8).abs_diff(d as u8) {
                    0 => 1,
                    1 => 1001,
                    2 => 2001,
                    3 => 1001,
                    _ => unreachable!()
                };
                (cost, st)
            })
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

    let mut dist_map = FxHashMap::default();
    let mut pq = BinaryHeap::new();
    let mut end_state = None;

    pq.push((Reverse(0usize), start, [None; 4]));
    while let Some((Reverse(dist), st @ (p, _), parents)) = pq.pop() {
        match dist_map.entry(st) {
            Entry::Vacant(e) if p == end => {
                e.insert((dist, parents));
                end_state.get_or_insert(st);
            }
            Entry::Vacant(e) => {
                e.insert((dist, parents));
                pq.extend({
                    grid.neighbors(st)
                        .map(|(w, nst)| (Reverse(dist + w), nst, [Some(st), None, None, None]))
                });
            }
            Entry::Occupied(mut e) => {
                let (ed, ep) = e.get_mut();
                if dist == *ed {
                    let i = ep.partition_point(|st| st.is_some());
                    let j = parents.partition_point(|st| st.is_some());
                    ep[i..i+j].copy_from_slice(&parents[..j]);
                }
            }
        }
    }

    let mut visited = Grid::new(false, grid.grid.len(), grid.grid[0].len());
    let mut frontier = Vec::from_iter(end_state);
    while let Some(node @ ((nr, nc), _)) = frontier.pop() {
        visited.grid[nr][nc] = true;
        if node == start { continue; }
        if let Some((_, parents)) = dist_map.get(&node) {
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