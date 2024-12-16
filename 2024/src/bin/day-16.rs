use std::cmp::Reverse;
use std::collections::hash_map::Entry;
use std::collections::{BinaryHeap, HashMap, HashSet};

fn main() {
    let input = std::fs::read_to_string("inputs/16.txt").unwrap();
    part1(&input);
    part2(&input);
}


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
fn part1(input: &str) {
    let Data { grid, start, end } = parse(input);

    let mut visited = HashSet::new();
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

    let p1 = result.unwrap();
    println!("{p1}");
}
fn part2(input: &str) {
    let Data { grid, start, end } = parse(input);

    let mut dist_map = HashMap::new();
    let mut pq = BinaryHeap::new();
    let mut end_state = None;

    pq.push((Reverse(0usize), start, vec![]));
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
                        .map(|(w, nst)| (Reverse(dist + w), nst, vec![st]))
                });
            }
            Entry::Occupied(mut e) => if dist == e.get().0 {
                e.get_mut().1.extend(parents);
            }
        }
    }

    let mut visited = HashSet::new();
    let mut frontier = Vec::from_iter(end_state);
    while let Some(node @ (np, _)) = frontier.pop() {
        visited.insert(np);
        if node == start { continue; }
        if let Some((_, parents)) = dist_map.get(&node) {
            frontier.extend_from_slice(parents);
        }
    }

    let p2 = visited.len();
    println!("{p2}");
}