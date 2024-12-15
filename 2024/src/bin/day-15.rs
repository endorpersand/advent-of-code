use std::collections::VecDeque;

fn main() {
    let input = std::fs::read_to_string("inputs/15.txt").unwrap();
    part1(&input);
    part2(&input);
}

type Position = (usize, usize);
type PosDelta = (isize, isize);

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
#[repr(u8)]
enum Tile {
    Wall = b'#', Box = b'O', None = b'.'
}
impl Tile {
    fn enlarge(self) -> [Tile2; 2] {
        match self {
            Tile::Wall => [Tile2::Wall, Tile2::Wall],
            Tile::Box  => [Tile2::BoxL, Tile2::BoxR],
            Tile::None => [Tile2::None, Tile2::None],
        }
    }
}
impl std::fmt::Display for Tile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", char::from(*self as u8))
    }
}
#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
#[repr(u8)]
enum Tile2 {
    Wall = b'#', BoxL = b'[', BoxR = b']', #[default] None = b'.'
}
impl std::fmt::Display for Tile2 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", char::from(*self as u8))
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Direction {
    Up, Right, Down, Left
}
impl Direction {
    fn delta(self) -> PosDelta {
        match self {
            Direction::Up    => (-1,  0),
            Direction::Right => ( 0,  1),
            Direction::Down  => ( 1,  0),
            Direction::Left  => ( 0, -1),
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
    grid: Vec<Vec<T>>,
    robot: Position
}
impl<T> Grid<T> {
    fn get(&self, (r, c): Position) -> Option<&T> {
        self.grid.get(r)?.get(c)
    }
    fn get_mut(&mut self, (r, c): Position) -> Option<&mut T> {
        self.grid.get_mut(r)?.get_mut(c)
    }
}
impl<T: std::fmt::Display> std::fmt::Display for Grid<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (r, row) in self.grid.iter().enumerate() {
            for (c, cell) in row.iter().enumerate() {
                if (r, c) == self.robot {
                    write!(f, "@")?;
                } else {
                    write!(f, "{cell}")?;
                }
            }
            writeln!(f)?;
        }
        Ok(())
    }
}

impl Grid<Tile> {
    fn shift(&mut self, dir: Direction) {
        let last = dir.ray(self.robot)
            .take_while(|&p| self.get(p).is_some_and(|&t| t == Tile::Box))
            .last()
            .unwrap_or(self.robot);
        let next = dir.translate(last);
        
        if self.get(next).is_some_and(|&t| t == Tile::None) {
            let first @ (fr, fc) = dir.translate(self.robot);
            let (nr, nc) = next;

            // Manual swap
            let ft = self.grid[fr][fc];
            self.grid[fr][fc] = self.grid[nr][nc];
            self.grid[nr][nc] = ft;
            //
            self.robot = first;
        }
    }
    fn score(&self) -> usize {
        self.grid.iter().enumerate()
            .flat_map(|(r, row)| {
                row.iter().enumerate()
                    .filter(|(_, &t)| t == Tile::Box)
                    .map(move |(c, _)| 100 * r + c)
            })
            .sum()
    }
}
impl Grid<Tile2> {
    fn shift(&mut self, dir: Direction) {
        let boxes = match dir {
            Direction::Right | Direction::Left => {
                dir.ray(self.robot)
                .take_while(|&p| self.get(p).is_some_and(|t| matches!(t, Tile2::BoxL | Tile2::BoxR)))
                .collect()
            },
            Direction::Up | Direction::Down => {
                // Rudimentary BFS
                let mut boxes = vec![];
                let mut frontier = VecDeque::from_iter([dir.translate(self.robot)]);
                while let Some(p) = frontier.pop_front() {
                    let pos = match self.get(p) {
                        Some(Tile2::BoxL) => [p, Direction::Right.translate(p)],
                        Some(Tile2::BoxR) => [Direction::Left.translate(p), p],
                        _ => continue,
                    };
                    for p in pos {
                        if !boxes.contains(&p) {
                            boxes.push(p);
                            frontier.push_back(dir.translate(p));
                        }
                    }
                }
                boxes
            },
        };
        
        let pushable = boxes.iter().chain([&self.robot])
            .all(|&p| self.get(dir.translate(p)).is_some_and(|&t| t != Tile2::Wall));
        if pushable {
            let box_pairs: Vec<_> = boxes.into_iter().map(|p| {
                let np = dir.translate(p);
                (np, std::mem::take(self.get_mut(p).unwrap()))
            }).collect();

            for ((r, c), t) in box_pairs {
                self.grid[r][c] = t;
            }
            self.robot = dir.translate(self.robot);
        }
    }
    fn score(&self) -> usize {
        self.grid.iter().enumerate()
            .flat_map(|(r, row)| {
                row.iter().enumerate()
                    .filter(|(_, &t)| t == Tile2::BoxL)
                    .map(move |(c, _)| 100 * r + c)
            })
            .sum()
    }
}

fn parse(input: &str) -> (Grid<Tile>, Vec<Direction>) {
    let mut lines = input.lines();

    let mut robot = None;
    let grid = (&mut lines).take_while(|l| !l.is_empty())
        .enumerate()
        .map(|(r, l)| l.bytes().enumerate().map(|(c, b)| match b {
            b'#' => Tile::Wall,
            b'O' => Tile::Box,
            b'@' => {
                robot.replace((r, c));
                Tile::None
            },
            b'.' => Tile::None,
            b => unreachable!("{}", char::from(b))
        }).collect()).collect();

    let directions = lines.flat_map(|l| l.bytes().map(|b| match b {
        b'^' => Direction::Up,
        b'>' => Direction::Right,
        b'v' => Direction::Down,
        b'<' => Direction::Left,
        b => unreachable!("{}", char::from(b))
    })).collect();

    (Grid { grid, robot: robot.unwrap() }, directions)
}

fn part1(input: &str) {
    let (mut grid, directions) = parse(input);
    for d in directions {
        grid.shift(d);
    }

    println!("{}", grid.score());
}
fn part2(input: &str) {
    let (Grid { grid, robot: (rr, rc) }, directions) = parse(input);

    // Mutate grid:
    let grid = grid.into_iter()
        .map(|row| row.into_iter().flat_map(Tile::enlarge).collect())
        .collect();
    let robot = (rr, 2 * rc);
    let mut grid = Grid { grid, robot };
    //

    for d in directions {
        // println!("{grid}");
        grid.shift(d);
    }
    // println!("{grid}");
    println!("{}", grid.score());
}