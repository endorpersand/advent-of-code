use std::collections::hash_map::Entry;
use std::collections::{HashMap, VecDeque};

fn main() {
    let input = std::fs::read_to_string("inputs/20.txt").unwrap();
    soln(&input);
}

type Position = (usize, usize);
type PosDelta = (isize, isize);

fn translate((r, c): Position, (dr, dc): PosDelta) -> Position {
    (r.wrapping_add_signed(dr), c.wrapping_add_signed(dc))
}
#[derive(Debug)]
struct Grid<T> {
    grid: Vec<Vec<T>>
}
impl<T> Grid<T> {
    fn get(&self, (r, c): Position) -> Option<&T> {
        self.grid.get(r)?.get(c)
    }
}
struct Data {
    grid: Grid<bool>,
    start: Position,
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

    Data { grid: Grid { grid }, start: start.unwrap(), end: end.unwrap() }
}
fn manhattan((r0, c0): Position, (r1, c1): Position) -> usize {
    r1.abs_diff(r0) + c1.abs_diff(c0)
}
/// Takes a list of positions (sorted by distance from start) 
/// and a produces (point 0, point 1, manhattan distance between points, time saved) tuple.
fn cheat_iter(spaces: &[Position]) -> impl Iterator<Item=(usize, usize, usize, usize)> + '_ {
    spaces.iter()
        .enumerate()
        .flat_map(|(i, &pi)| {
            std::iter::zip((i + 1).., &spaces[(i + 1)..]).map(move |(j, &pj)| {
                let dist = manhattan(pi, pj);
                let time = j.saturating_sub(i).saturating_sub(dist);
                (i, j, dist, time)
            })
        })
}
fn soln(input: &str) {
    let Data { grid, start, end } = parse(input);

    let mut dist_map: HashMap<_, _> = HashMap::from_iter([(start, 0)]);
    let mut queue = VecDeque::from([start]);
    while let Some(p) = queue.pop_front() {
        if p == end { break; }

        let d = dist_map[&p];
        [(0, 1), (1, 0), (0, -1), (-1, 0)]
            .into_iter()
            .map(|d| translate(p, d))
            .filter(|&np| grid.get(np).is_some_and(|&n| !n))
            .for_each(|np| if let Entry::Vacant(e) = dist_map.entry(np) {
                e.insert(d + 1);
                queue.push_back(np);
            });
    }

    let mut spaces: Vec<_> = dist_map.keys().copied().collect();
    spaces.sort_by_key(|p| dist_map[p]);

    let p1 = cheat_iter(&spaces)
        .filter(|&(_, _, d, t)| d == 2 && t >= 100) // limit distance, saves at least 100 ps of time
        .count();
    println!("{p1}");

    let p2 = cheat_iter(&spaces)
        .filter(|&(_, _, d, t)| d <= 20 && t >= 100) // limit distance, saves at least 100 ps of time
        .count();
    println!("{p2}");
}