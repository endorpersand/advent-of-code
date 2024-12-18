use std::collections::{HashSet, VecDeque};

fn main() {
    let input = std::fs::read_to_string("inputs/18.txt").unwrap();
    soln(&input);
}

type Position = (usize, usize);
const GRID_SIZE: usize = 71;
const CORRUPT_COUNT: usize = 1024;
const START: Position = (0, 0);
const END: Position = (GRID_SIZE - 1, GRID_SIZE - 1);

fn bfs(grid: &[[bool; GRID_SIZE]], start: Position, end: Position) -> Option<usize> {
    let mut visited: HashSet<_> = HashSet::from_iter([start]);
    let mut frontier = VecDeque::from_iter([(start, 0)]);

    while let Some((pos @ (x, y), dist)) = frontier.pop_front() {
        if pos == end {
            return Some(dist);
        }

        frontier.extend({
            [(-1, 0), (0, 1), (1, 0), (0, -1)].into_iter()
                .map(move |(dx, dy)| (x.wrapping_add_signed(dx), y.wrapping_add_signed(dy)))
                .filter(|&(x, y)| grid.get(x).and_then(|r| r.get(y)).is_some_and(|c| !c))
                .filter(|&p| visited.insert(p))
                .map(|p| (p, dist + 1))
        })
    }

    None
}

fn soln(input: &str) {
    let positions: Vec<Position> = input.lines()
        .map(|s| s.split_once(',').unwrap())
        .map(|(xs, ys)| (xs.parse().unwrap(), ys.parse().unwrap()))
        .collect();
    let (left, right) = positions.split_at(CORRUPT_COUNT);

    let mut grid = vec![[false; GRID_SIZE]; GRID_SIZE];
    left.iter().for_each(|&(x, y)| grid[x][y] = true);

    // part 1
    println!("{}", bfs(&grid, START, END).unwrap());

    // part 2
    println!("{:?}", {
        right.iter().find(|&&(x, y)| {
            grid[x][y] = true;
            bfs(&grid, START, END).is_none()
        }).unwrap()
    });
}