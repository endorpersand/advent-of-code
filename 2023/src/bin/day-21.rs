use std::collections::HashSet;

const STEPS: usize = 26_501_365;
fn main() {
    let mut txt = std::fs::read_to_string("inputs/21.txt").unwrap();
    if !txt.ends_with('\n') { txt.push('\n') };
    let grid = Grid::parse(&txt);

    // PART A
    // println!("{}", find_nei_steps(&grid, 64));
    
    // PART B
    let grid2 = Grid2::parse(&txt);
    let steps = CachedSteps::generate(&grid);
    // assert_eq!(steps.calculate_spots(&grid, 262), 62655);
    // let mut map = find_nei_steps2(&grid2, 129);
    // for i in 130..=700 {
    //     println!("checking {i}");
    //     assert_eq!(find_nei_steps2_next(&grid2, &mut map), steps.calculate_spots(&grid, i));
    // }
    println!("{}", steps.calculate_spots(&grid, STEPS));
}

// PART A
#[derive(Debug)]
struct Grid<'s> {
    buffer: &'s [u8],
    // cols is 1 greater than number of cols
    // because it also contains new line
    cols: usize,
    rows: usize
}
impl<'s> Grid<'s> {
    fn parse(file: &'s str) -> Self {
        let cols = file.find('\n').unwrap() + 1;
        let rows = file.len() / cols;

        Self { buffer: file.as_bytes(), cols, rows }
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
        // dbg!(i + 1, frontier.len());
    }
    
    frontier.len()
}

// PART B
#[derive(Debug)]
struct CachedSteps {
    center: Vec<usize>,
    edges: Vec<usize>,
    corners: Vec<usize>,
}

fn find_nei_steps_at(g: &Grid, n: usize, start: usize) -> Vec<usize> {
    let mut ctr = Vec::with_capacity(n);
    
    let mut frontier = HashSet::new();
    frontier.insert(start);
    ctr.push(frontier.len());

    for _ in 1..=n {
        for tile in std::mem::take(&mut frontier) {
            frontier.extend(g.neighbors(tile).filter(|&i| g.buffer[i] != b'#'));
        }
        ctr.push(frontier.len());
    }
    
    ctr
}

impl CachedSteps {
    fn generate(g: &Grid) -> Self {
        let mid = g.rows / 2;
        let end = g.rows - 1;

        let center_dist = 2 * mid;
        let edge_dist = 3 * mid;
        let corner_dist = 4 * mid;
    
        let idx = |r, c| r * g.cols + c;
        CachedSteps {
            center: find_nei_steps_at(g, center_dist, idx(mid, mid)),
            edges: {
                find_nei_steps_at(g, edge_dist, idx(mid, 0))
                    .into_iter()
                    .zip(find_nei_steps_at(g, edge_dist, idx(0,   mid)))
                    .zip(find_nei_steps_at(g, edge_dist, idx(end, mid)))
                    .zip(find_nei_steps_at(g, edge_dist, idx(mid, end)))
                    .map(|(((a, b), c), d)| a + b + c + d)
                    .collect()
            },
            corners: {
                find_nei_steps_at(g, corner_dist, idx(0,   0))
                    .into_iter()
                    .zip(find_nei_steps_at(g, corner_dist, idx(end, 0)))
                    .zip(find_nei_steps_at(g, corner_dist, idx(0,   end)))
                    .zip(find_nei_steps_at(g, corner_dist, idx(end, end)))
                    .map(|(((a, b), c), d)| a + b + c + d)
                    .collect()
            }
        }
    }

    fn calculate_spots(&self, g: &Grid, n: usize) -> usize {
        let size = g.rows;
        let mid = size / 2;

        // it's all encompassed in one square
        if n <= mid { return self.center[n] };
        // it's all encompassed in the area surrounding the square
        if n < size {
            let excess = n - mid;
            return self.center[n] + self.edges[excess - 1];
        };


        let rad = (n / size) - 1;
        let center = 1;
        
        let center_is_odd = n % 2 != 0;
        let inner_rings = {
            let n_rings = rad >> 1;
            n_rings * (n_rings + 1)
        };
        let outer_rings = rad * (rad + 1) / 2 - inner_rings;
        let (odds, evens) = match center_is_odd {
            true  => (outer_rings, inner_rings),
            false => (inner_rings, outer_rings),
        };
        let lcorner = rad.max(1);
        let scorner = if n > size * (rad.max(1) + 1) { rad.max(1) + 1 } else { 0 };
        let ledges = 1;
        let sedges = usize::from(n > mid + size * (rad + 1));

        let lcorner_dist = n - size * rad.max(1);
        let scorner_dist = if scorner > 0 { n - size * (rad.max(1) + 1) } else { 0 };
        let ledge_dist   = n - mid - size * rad;
        let sedge_dist   = if sedges > 0 { n - mid - size * (rad + 1) } else { 0 };
        
        // dbg!(n, center, rad, odds, evens, lcorner, scorner, ledges, sedges, lcorner_dist, scorner_dist, ledge_dist, sedge_dist);
        {
              center  * self.center[self.center.len() - (1 << u8::from(center_is_odd))]
            + odds    * self.edges[self.edges.len() - 1]
            + evens   * self.edges[self.edges.len() - 2]
            + if lcorner_dist > 0 { lcorner * self.corners[lcorner_dist - 1] } else { 0 }
            + if scorner_dist > 0 { scorner * self.corners[scorner_dist - 1] } else { 0 }
            + if ledge_dist   > 0 { ledges  * self.edges[ledge_dist - 1]     } else { 0 }
            + if sedge_dist   > 0 { sedges  * self.edges[sedge_dist - 1]     } else { 0 }
        }
    }
}

#[derive(Debug)]
struct Grid2 {
    buffer: Vec<u8>,
    cols: usize,
    rows: usize
}
impl Grid2 {
    fn parse(file: &str) -> Self {
        let buffer: Vec<_> = file.bytes()
            .filter(|&b| b != b'\n')
            .collect();
        let cols = file.find('\n').unwrap();
        let rows = buffer.len() / cols;

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
}

fn find_nei_steps2(g: &Grid2, n: usize) -> HashSet<(isize, isize)> {
    let start = g.buffer.iter()
        .position(|&t| t == b'S')
        .unwrap();
    let mut frontier = HashSet::new();
    frontier.insert(((start / g.cols) as isize, (start % g.cols) as isize));
    
    for _ in 0..n {
        find_nei_steps2_next(g, &mut frontier);
    }
    frontier
}
fn find_nei_steps2_next(g: &Grid2, frontier: &mut HashSet<(isize, isize)>) -> usize {
    for tile in std::mem::take(frontier) {
        frontier.extend(g.neighbors(tile).filter(|&i| g.index(i) != b'#'));
    }
    frontier.len()
}