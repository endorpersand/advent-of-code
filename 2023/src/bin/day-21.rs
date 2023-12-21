use std::collections::HashSet;

const STEPS: usize = 26_501_365;
fn main() {
    let txt = std::fs::read_to_string("inputs/21.txt").unwrap();
    
    // PART A
    let grid = Grid::parse(&txt);
    // println!("{}", find_nei_steps(&grid, 64));
    
    // PART B
    println!("{}", find_nei_steps(&grid, 136));
    let grid = Grid2::parse(&txt);
    // println!("{}", find_nei_steps2(&grid, 65));
    // println!("{}", find_nei_steps3(&grid, 65));
}

// PART A
#[derive(Debug)]
struct Grid<'s> {
    buffer: &'s [u8],
    cols: usize
}
impl<'s> Grid<'s> {
    fn parse(file: &'s str) -> Self {
        let cols = file.find('\n').unwrap() + 1;

        Self { buffer: file.as_bytes(), cols }
    }

    fn neighbors(&self, i: usize) -> impl Iterator<Item=usize> + '_ {
        [1isize, -1, (self.cols as isize), -(self.cols as isize)]
            .into_iter()
            .map(move |j| i.wrapping_add_signed(j))
            .filter(|nei| (0..self.buffer.len()).contains(nei))
            .filter(|&nei| self.buffer[nei] != b'\n')
    }
}

fn find_nei_steps(g: &Grid, n: usize) -> usize {
    let start = g.buffer.iter()
        .position(|&t| t == b'S')
        .unwrap();
    let mut frontier = HashSet::new();
    frontier.insert(start);

    for i in 0..n {
        for tile in std::mem::take(&mut frontier) {
            frontier.extend(g.neighbors(tile).filter(|&i| g.buffer[i] != b'#'));
        }
        dbg!(i + 1, frontier.len());
    }
    
    frontier.len()
}

// PART B

#[derive(Debug)]
struct Grid2 {
    buffer: Vec<u8>,
    cols: usize,
    rows: usize
}
impl Grid2 {
    fn parse(file: &str) -> Self {
        let buffer = file.bytes()
            .filter(|&b| b != b'\n')
            .collect();
        let cols = file.find('\n').unwrap();
        let rows = file.len() / cols;

        Self { buffer, cols, rows }
    }

    fn index(&self, (r, c): (isize, isize)) -> u8 {
        let nr = r.rem_euclid(self.rows as isize) as usize;
        let nc = c.rem_euclid(self.cols as isize) as usize;
        self.buffer[nr * self.cols + nc]
    }

    fn neighbors(&self, (r, c): (isize, isize)) -> impl Iterator<Item=(isize, isize)> + '_ {
        [(0, 1), (0, -1), (-1, 0), (1, 0)]
            .into_iter()
            .map(move |(dr, dc)| (r.wrapping_add(dr), c.wrapping_add(dc)))
    }
    fn dist_away(&self, (r, c): (isize, isize), dist: usize) -> impl Iterator<Item=(isize, isize)> + '_ {
        let dist = dist as isize;

        (0isize..dist)
            .flat_map(move |i| [(dist - i, i), (-i, dist - i), (i - dist, -i), (i, i - dist)])
            .map(move |(dr, dc)| (r.wrapping_add(dr), c.wrapping_add(dc)))
    }
}

fn find_nei_steps2(g: &Grid2, n: usize) -> usize {
    let start = g.buffer.iter()
        .position(|&t| t == b'S')
        .unwrap();
    let mut frontier = HashSet::new();
    frontier.insert(((start / g.cols) as isize, (start % g.cols) as isize));

    for i in 0..n {
        for tile in std::mem::take(&mut frontier) {
            frontier.extend(g.neighbors(tile).filter(|&i| g.index(i) != b'#'));
        }
        dbg!((i + 1, frontier.len()));
    }
    frontier.len()
}

fn find_nei_steps3(g: &Grid2, n: usize) -> usize {
    let start = g.buffer.iter()
        .position(|&t| t == b'S')
        .unwrap();
    let start = ((start / g.cols) as isize, (start % g.cols) as isize);

    let mut traversed = 0;

    for i in 0..=(n / 2) {
        let new = g.dist_away(start, i * 2 + 1)
            .filter(|&i| g.index(i) != b'#')
            .count();

        traversed += new;
        // dbg!((i * 2, traversed));
    }
    
    traversed
}
