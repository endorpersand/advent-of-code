use std::cmp::Reverse;
use std::collections::hash_map::Entry;
use std::collections::{BinaryHeap, HashMap, HashSet, VecDeque};

fn main() {
    let input = std::fs::read_to_string("inputs/16.txt").unwrap();
    // part1(&input);
    part2(&input);
}


type Position = (usize, usize);
type PosDelta = (isize, isize);

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

    fn ray(self, start: Position) -> impl Iterator<Item=Position> {
        std::iter::successors(Some(self.translate(start)), move |&p| Some(self.translate(p)))
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
struct State {
    grid: Grid<bool>,
    start: Position,
    end: Position,
    dir: Direction
}

fn parse(input: &str) -> State {
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

    State { grid: Grid { grid }, start: start.unwrap(), end: end.unwrap(), dir: Direction::East }
}
fn part1(input: &str) {
    let State { grid, start, end, dir } = parse(input);

    let mut dist = HashMap::new();
    let mut pq = BinaryHeap::new();
    let mut result = None;

    pq.push((Reverse(0usize), start, dir));
    while let Some((Reverse(d), p, dir)) = pq.pop() {
        if p == end {
            result.replace(d);
            break;
        }
        if let Entry::Vacant(e) = dist.entry((p, dir)) {
            e.insert(d);
            
            pq.extend({
                [Direction::North, Direction::East, Direction::South, Direction::West]
                    .into_iter()
                    .map(|nd| (nd, nd.translate(p)))
                    .filter(|&(_, np)| grid.get(np).is_some_and(|c| !c))
                    .map(|(nd, np)| {
                        let weight = match (nd as u8).abs_diff(dir as u8) {
                            0 => 1,
                            1 => 1001,
                            2 => 2001,
                            3 => 1001,
                            _ => unreachable!()
                        };
                        (Reverse(d + weight), np, nd)
                    })
            });
        }
    }

    let p1 = result.unwrap();
    println!("{p1}");
}
fn part2(input: &str) {
    let State { grid, start, end, dir } = parse(input);

    let mut dist = HashMap::new();
    let mut pq = BinaryHeap::new();

    pq.push((Reverse(0usize), start, dir, vec![]));
    while let Some((Reverse(d), p, dir, parent)) = pq.pop() {
        match dist.entry((p, dir)) {
            Entry::Vacant(e) => {
                e.insert((d, parent));
                if p == end { continue; }

                pq.extend({
                    [Direction::North, Direction::East, Direction::South, Direction::West]
                        .into_iter()
                        .map(|nd| (nd, nd.translate(p)))
                        .filter(|&(_, np)| grid.get(np).is_some_and(|c| !c))
                        .map(|(nd, np)| {
                            let weight = match (nd as u8).abs_diff(dir as u8) {
                                0 => 1,
                                1 => 1001,
                                2 => 2001,
                                3 => 1001,
                                _ => unreachable!()
                            };
                            (Reverse(d + weight), np, nd, vec![(p, dir)])
                        })
                });
            }
            Entry::Occupied(mut e) => {
                assert!(d >= e.get().0);
                if d == e.get().0 {
                    e.get_mut().1.extend(parent);
                }
            }
        }
    }

    let mut visited: HashSet<_> = HashSet::new();
    let (&end_node, _) = dist.iter()
        .filter(|&(&(p, _), _)| p == end)
        .min_by_key(|(_, (d, _))| d)
        .unwrap();
    let mut frontier = VecDeque::from_iter([end_node]);
    while let Some(node @ (np, _)) = frontier.pop_front() {
        visited.insert(np);
        if np != start {
            frontier.extend(dist.get(&node).map_or(&vec![], |d| &d.1).iter().copied());
        }
    }

    let p2 = visited.len();
    println!("{p2}");
}