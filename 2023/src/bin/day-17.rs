use std::cmp::Reverse;
use std::collections::{HashMap, BinaryHeap, HashSet};

fn main() {
    let txt = std::fs::read_to_string("inputs/17t.txt").unwrap();
    let grid = Grid::parse(&txt);

    println!("{}", find_minimum(&grid, (0, 0), (grid.rows - 1, grid.cols - 1)));
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

    fn get(&self, (r, c): (usize, usize)) -> Option<u8> {
        self.buffer.get(r * self.cols + c).copied()
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
fn get_next_steps(grid: &Grid, step: Step) -> Vec<Step> {
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
fn find_minimum(grid: &Grid, start: (usize, usize), end: (usize, usize)) -> usize {
    let mut visited = HashMap::new();
    let mut parents = HashMap::new();

    let start_step = Step { index: start, forward_steps: 0, dir: Dir::Right };
    visited.insert(start_step, 0);
    
    let mut frontier = BinaryHeap::new();
    frontier.extend({
        get_next_steps(grid, start_step)
            .into_iter()
            .map(|ns| (Reverse(grid.index(ns.index) as usize), ns))
    });

    while let Some((Reverse(loss), step)) = frontier.pop() {
        if visited.contains_key(&step) { println!("you hit {step:?} already"); };
        if step.index == end {
            // traversal!
            let mut trav = vec![step];
            while let Some(&parent) = parents.get(trav.last().unwrap()) {
                trav.push(parent);
            }
            for t in trav.iter().rev() { println!("{t:?}") };

            return loss;
        };

        frontier.extend({
            get_next_steps(grid, step)
                .into_iter()
                .filter(|s| !visited.contains_key(s))
                .inspect(|&ns| { parents.insert(ns, step); })
                .map(|ns| (Reverse(loss + grid.index(ns.index) as usize), ns))
        });
        
        // clean frontier
        let mut in_frontier = HashSet::new();
        frontier = std::mem::take(&mut frontier)
            .into_sorted_vec()
            .into_iter()
            .rev()
            .filter(|&(_, s)| in_frontier.insert(s))
            .collect();

        visited.insert(step, loss);
    }

    println!("{visited:?}");
    panic!("should have found {end:?}");
}