use std::collections::HashSet;
use std::fmt::Write;
use std::fs;
use std::str::FromStr;

fn main() {
    let input = fs::read_to_string("inputs/24.txt").unwrap();
    let mut grid = input.parse::<Grid>().unwrap();

    let mut sum = grid.calc_path_len(grid.start, grid.end);
    println!("{sum}");
    sum += grid.calc_path_len(grid.end, grid.start);
    sum += grid.calc_path_len(grid.start, grid.end);
    println!("{sum}");
}

type Coord = (isize, isize);

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Direction {
    Right, Up, Left, Down
}

macro_rules! make_mask {
    ($($($l:literal)*),+) => {
        [
            $(
                &[
                    $(Direction::ORDER[$l]),*
                ]
            ),+
        ]
    }
}
impl Direction {
    const ORDER: [Direction; 4] = [Direction::Right, Direction::Up, Direction::Left, Direction::Down];
    const MASK: [&'static [Direction]; 16] = make_mask![
               ,
        0      ,
          1    ,
        0 1    ,
            2  ,
        0   2  ,
          1 2  ,
        0 1 2  ,
              3,
        0     3,
          1   3,
        0 1   3,
            2 3,
        0   2 3,
          1 2 3,
        0 1 2 3
    ];

    fn from_u8(mask: u8) -> &'static [Direction] {
        Direction::MASK[mask as usize]
    }

    const DELTA: [Coord; 4] = [
        ( 0,  1),
        (-1,  0),
        ( 0, -1),
        ( 1,  0),
    ];

    fn apply_delta_unbounded(&self, (r, c): Coord) -> Coord {
        let (dr, dc) = Direction::DELTA[*self as usize];

        (r + dr, c + dc)
    }
    fn apply_delta_rem(&self, (r, c): Coord, (maxr, maxc): Coord) -> (usize, usize) {
        let (dr, dc) = Direction::DELTA[*self as usize];

        ((r + dr).rem_euclid(maxr) as _, (c + dc).rem_euclid(maxc) as _)
    }
}

#[derive(Debug)]
struct Grid {
    grid: Vec<Vec<u8>>, // bit mask representing direction
    rows: usize,
    cols: usize,

    start: Coord,
    end: Coord
}

impl Grid {
    fn zeroed(&self) -> Vec<Vec<u8>> {
        std::iter::from_fn(|| {
            let mut vec = vec![];
            vec.resize(self.cols, 0);
            Some(vec)
        })
            .take(self.rows)
            .collect()
    }

    #[inline]
    fn dims(&self) -> Coord {
        (self.rows as _, self.cols as _)
    }

    fn apply_mask_at(&mut self, at: Coord, mask: u8) {
        for &d in Direction::from_u8(mask) {
            let (r, c) = d.apply_delta_rem(at, self.dims());
            self.grid[r][c] |= 1 << (d as u8);
        }
    }

    fn progress(&mut self) {
        let new_grid = self.zeroed();
        let old_grid = std::mem::replace(&mut self.grid, new_grid);

        for (at, mask) in flatten(old_grid) {
            self.apply_mask_at(at, mask);
        }
    }

    fn traverseable(&self, coord @ (r, c): Coord) -> bool {
        
        coord == self.start || coord == self.end || {
            (0..(self.rows as _)).contains(&r)
            && (0..(self.cols as _)).contains(&c)
            && self.grid[r as usize][c as usize] == 0
        }
    }

    fn can_traverse(&self, at: Coord) -> Vec<Coord> {
        Direction::ORDER.into_iter()
            .map(|d| d.apply_delta_unbounded(at))
            .chain(std::iter::once(at))
            .filter(|&coord| self.traverseable(coord))
            .collect()
    }

    fn calc_path_len(&mut self, start: Coord, end: Coord) -> usize {
        let mut steps = 0;
        let mut frontier: HashSet<Coord> = HashSet::new(); // tails of paths
        frontier.insert(start);
    
        while !frontier.contains(&end) {
            // println!("Completed: {steps}, frontier: {frontier:?}");
    
            assert!(!frontier.is_empty());

            self.progress();
            // println!("{self}");
            frontier = frontier.into_iter()
                .flat_map(|at| self.can_traverse(at))
                .collect();
            
            steps += 1;
        }
        // println!("{self}");
        // println!("{:?}", frontier[&end]);
        steps
    }
}

fn flatten(grid: Vec<Vec<u8>>) -> Vec<(Coord, u8)> {
    grid.into_iter()
        .enumerate()
        .flat_map(|(r, row)| {
            row.into_iter()
               .enumerate()
               .filter_map(move |(c, e)| (e != 0).then_some(((r as _, c as _), e)))
        })
        .collect()
}

impl FromStr for Grid {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut lines = s.lines();
        // TODO, handle beginning and end
        lines.next();
        lines.next_back();

        let grid: Vec<Vec<_>> = lines.map(|line| {
            line[1..(line.len() - 1)].chars().map(|c| {
                match c {
                    '.' => 0,
                    '<' => 1 << (Direction::Left as u8),
                    '^' => 1 << (Direction::Up as u8),
                    '>' => 1 << (Direction::Right as u8),
                    'v' => 1 << (Direction::Down as u8),
                    _ => unimplemented!()
                }
            }).collect()
        }).collect();

        let rows = grid.len();
        let cols = grid[0].len();
        Ok(Grid {
            grid,
            rows,
            cols,

            start: (-1, 0),
            end: (rows as isize, cols as isize - 1)
        })
    }
}

impl std::fmt::Display for Grid {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.grid.iter()
            .try_for_each(|row| {
                row.iter().try_for_each(|&e| {
                    let bits = e.count_ones();
                    if bits > 1 { write!(f, "{bits:?}") }
                    else if bits == 0 { f.write_char('.') }
                    else {
                        match Direction::MASK[e as usize] {
                            [Direction::Right] => f.write_char('>'),
                            [Direction::Up]    => f.write_char('^'),
                            [Direction::Left]  => f.write_char('<'),
                            [Direction::Down]  => f.write_char('v'),
                            _ => unreachable!()
                        }
                    }
                })?;
                writeln!(f)
            })
    }
}