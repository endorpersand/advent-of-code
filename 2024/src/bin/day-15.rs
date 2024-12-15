use std::collections::VecDeque;

fn main() {
    let input = std::fs::read_to_string("inputs/15.txt").unwrap();
    // part1(&input);
    part2(&input);
}

type Position = (usize, usize);
type PosDelta = (isize, isize);

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Tile {
    Wall, Box, None
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
        match self {
            Tile::Wall => write!(f, "#"),
            Tile::Box  => write!(f, "O"),
            Tile::None => write!(f, "."),
        }
    }
}
#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
enum Tile2 {
    Wall, BoxL, BoxR, #[default] None
}
impl std::fmt::Display for Tile2 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Tile2::Wall => write!(f, "#"),
            Tile2::BoxL => write!(f, "["),
            Tile2::BoxR => write!(f, "]"),
            Tile2::None => write!(f, "."),
        }
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

    fn ray(self) -> impl Iterator<Item=PosDelta> {
        let (dr, dc) = self.delta();
        (1..).map(move |n| (dr * n, dc * n))
    }
}
fn translate((r, c): Position, (dr, dc): PosDelta) -> Position {
    (r.wrapping_add_signed(dr), c.wrapping_add_signed(dc))
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
impl Grid<Tile> {
    fn shift(&mut self, dir: Direction) {
        let boxes: Vec<_> = dir.ray()
            .map(|d| translate(self.robot, d))
            .take_while(|&p| self.get(p).is_some_and(|&t| t == Tile::Box))
            .collect();

        let last = boxes.last().copied().unwrap_or(self.robot);
        let pushable = self.get(translate(last, dir.delta())).is_some_and(|&t| t == Tile::None);
        if pushable {
            for bp in boxes.into_iter().rev() {
                *self.get_mut(translate(bp, dir.delta())).unwrap() = Tile::Box;
            }
            self.robot = translate(self.robot, dir.delta());
            *self.get_mut(self.robot).unwrap() = Tile::None;
        }
    }
    fn score(&self) -> usize {
        self.grid.iter()
            .enumerate()
            .flat_map(|(r, row)| {
                row.iter()
                    .enumerate()
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
                dir.ray()
                .map(|d| translate(self.robot, d))
                .take_while(|&p| self.get(p).is_some_and(|t| matches!(t, Tile2::BoxL | Tile2::BoxR)))
                .collect()
            },
            Direction::Up | Direction::Down => {
                let mut boxes = vec![];
                let mut frontier = VecDeque::from_iter([translate(self.robot, dir.delta())]);
                while let Some(p) = frontier.pop_front() {
                    let pos = match self.get(p) {
                        Some(Tile2::BoxL) => [p, translate(p, Direction::Right.delta())],
                        Some(Tile2::BoxR) => [translate(p, Direction::Left.delta()), p],
                        _ => continue,
                    };
                    for p in pos {
                        if !boxes.contains(&p) {
                            boxes.push(p);
                            frontier.push_back(translate(p, dir.delta()));
                        }
                    }
                }
                boxes
            },
        };
        
        let pushable = boxes.iter().chain([&self.robot])
            .all(|&p| self.get(translate(p, dir.delta())).is_some_and(|&t| t != Tile2::Wall));
        if pushable {
            let box_pairs: Vec<_> = boxes.into_iter().map(|p| {
                let np = translate(p, dir.delta());
                (np, std::mem::take(self.get_mut(p).unwrap()))
            }).collect();

            for ((r, c), t) in box_pairs {
                self.grid[r][c] = t;
            }
            self.robot = translate(self.robot, dir.delta());
        }
    }
    fn score(&self) -> usize {
        self.grid.iter()
            .enumerate()
            .flat_map(|(r, row)| {
                row.iter()
                    .enumerate()
                    .filter(|(_, &t)| t == Tile2::BoxL)
                    .map(move |(c, _)| 100 * r + c)
            })
            .sum()
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
            _ => unreachable!()
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