use std::cmp::Reverse;
use std::collections::{HashMap, BinaryHeap};

fn main() {
    let txt = std::fs::read_to_string("inputs/17.txt").unwrap();
    let grid = Grid::parse(&txt);
    
    let end = (grid.rows - 1, grid.cols - 1);
    println!("{}", find_minimum(&grid, (0, 0), |s| s.index == end, next_step_a));
    println!("{}", find_minimum(&grid, (0, 0), |s| s.index == end && s.forward_steps >= 4, next_step_b));
}

struct Grid {
    buffer: Vec<u8>,
    cols: usize,
    rows: usize
}
impl Grid {
    fn parse(file: &str) -> Self {
        let buffer: Vec<_> = file.bytes()
            .filter(|&b| b != b'\n')
            .map(|b| b - b'0')
            .collect();

        let cols = file.find('\n').unwrap();
        let rows = buffer.len() / cols;

        Self { buffer, cols, rows }
    }

    fn in_bounds(&self, (r, c): (usize, usize)) -> bool {
        (0..self.rows).contains(&r) && (0..self.cols).contains(&c)
    }

    fn index(&self, (r, c): (usize, usize)) -> u8 {
        self.buffer[r * self.cols + c]
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
#[allow(unused)]
#[repr(u8)]
enum Dir {
    Up = 0, Right = 1, Down = 2, Left = 3
}
impl Dir {
    fn delta(self) -> (isize, isize) {
        match self {
            Dir::Up    => (-1,  0),
            Dir::Right => ( 0,  1),
            Dir::Down  => ( 1,  0),
            Dir::Left  => ( 0, -1),
        }
    }
    fn rot_left(self) -> Dir {
        unsafe { std::mem::transmute::<u8, Dir>((self as u8).wrapping_sub(1) & 0b11) }
    }
    fn rot_right(self) -> Dir {
        unsafe { std::mem::transmute::<u8, Dir>((self as u8).wrapping_add(1) & 0b11) }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
struct Step {
    index: (usize, usize),
    forward_steps: u8,
    dir: Dir
}

fn next_step_a(grid: &Grid, step: Step) -> Vec<Step> {
    let Step { index: (r, c), forward_steps, dir: curr_dir } = step;
    
    let mut steps = Vec::with_capacity(4);

    let options = [
        Some(curr_dir.rot_left()), 
        Some(curr_dir.rot_right()), 
        (forward_steps < 3).then_some(curr_dir)
    ]
        .into_iter()
        .flatten();
    for d in options {
        let (dr, dc) = d.delta();
        let index = (r.wrapping_add_signed(dr), c.wrapping_add_signed(dc));

        if grid.in_bounds(index) {
            steps.push(Step {
                index,
                forward_steps: if d == curr_dir { forward_steps + 1 } else { 1 },
                dir: d
            });
        }
    }
    
    steps
}
fn next_step_b(grid: &Grid, step: Step) -> Vec<Step> {
    let Step { index: (r, c), forward_steps, dir: curr_dir } = step;
    
    let mut steps = Vec::with_capacity(4);

    let options = [
        (forward_steps >= 4).then_some(curr_dir.rot_left()),
        (forward_steps >= 4).then_some(curr_dir.rot_right()),
        (forward_steps < 10).then_some(curr_dir)
    ]
        .into_iter()
        .flatten();
    for d in options {
        let (dr, dc) = d.delta();
        let index = (r.wrapping_add_signed(dr), c.wrapping_add_signed(dc));

        if grid.in_bounds(index) {
            steps.push(Step {
                index,
                forward_steps: if d == curr_dir { forward_steps + 1 } else { 1 },
                dir: d
            });
        }
    }
    
    steps
}
fn find_minimum<FEnd, FNext>(grid: &Grid, start: (usize, usize), mut end: FEnd, mut next: FNext) -> usize
    where FEnd: FnMut(Step) -> bool,
          FNext: FnMut(&Grid, Step) -> Vec<Step>
{

    let mut visited = HashMap::new();
    let mut parents = HashMap::new();

    let mut frontier = BinaryHeap::new();
    frontier.push((Reverse(0), Step { index: start, forward_steps: 0, dir: Dir::Right }));
    frontier.push((Reverse(0), Step { index: start, forward_steps: 0, dir: Dir::Down  }));

    while let Some((Reverse(loss), step)) = frontier.pop() {
        if end(step) {
            // traversal!
            let mut trav = vec![step];
            while let Some(&parent) = parents.get(trav.last().unwrap()) {
                trav.push(parent);
            }
            for t in trav.iter().rev() { println!("{t:?}") };
            return loss;
        };

        if visited.contains_key(&step) { continue; }

        frontier.extend({
            next(grid, step)
                .into_iter()
                .filter(|ns| !visited.contains_key(ns))
                .inspect(|&ns| { parents.insert(ns, step); })
                .map(|ns| {
                    let nl = loss + grid.index(ns.index) as usize;
                    (Reverse(nl), ns)
                })
        });

        visited.insert(step, loss);
    }

    panic!("should have found end");
}