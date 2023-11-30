use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::fs;
use std::str::FromStr;

use regex::Regex;

fn main() {
    let input = fs::read_to_string("inputs/22.txt").unwrap();

    let move_str = input.lines().next_back().unwrap();
    let moves: Vec<_> = Regex::new(r"(:?L|R|\d+)").unwrap().find_iter(move_str)
        .map(|m| {
            let ms = m.as_str();
            if let Ok(n) = ms.parse() {
                Move::Forward(n)
            } else if ms == "L" {
                Move::Left
            } else if ms == "R" {
                Move::Right
            } else {
                unimplemented!()
            }
        })
        .collect();
        
    let mut board = input.parse::<Board>().unwrap();
    // println!("{:?}", board.pos);
    for m in moves {
        board.apply_move(m);
        // println!("{:?}", board.pos);
    }
    println!("{:?}", board.pos.password(&board.cubemap));
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Tile {
    Out,
    Walk,
    Rock
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Move {
    Forward(usize),
    Left,
    Right
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Direction {
    Right = 0, Down = 1, Left = 2, Up = 3
}
impl Direction {
    fn delta(&self) -> Coord {
        match self {
            Direction::Left  => FACING_LEFT,
            Direction::Right => FACING_RIGHT,
            Direction::Up    => FACING_UP,
            Direction::Down  => FACING_DOWN,
        }
    }
    fn rotate_left(&self) -> Direction {
        match self {
            Direction::Left  => Direction::Down,
            Direction::Right => Direction::Up,
            Direction::Up    => Direction::Left,
            Direction::Down  => Direction::Right,
        }
    }
    fn rotate_right(&self) -> Direction {
        match self {
            Direction::Left  => Direction::Up,
            Direction::Right => Direction::Down,
            Direction::Up    => Direction::Right,
            Direction::Down  => Direction::Left,
        }
    }
    fn invert(&self) -> Direction {
        match self {
            Direction::Right => Direction::Left,
            Direction::Down => Direction::Up,
            Direction::Left => Direction::Right,
            Direction::Up => Direction::Down,
        }
    }
}
type CubeCoords = (usize, usize);
type Coord = (isize, isize);
const FACING_RIGHT: Coord = ( 0,  1);
const FACING_LEFT: Coord  = ( 0, -1);
const FACING_UP: Coord    = (-1,  0);
const FACING_DOWN: Coord  = ( 1,  0);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Position {
    cube: usize,
    pos: Coord,
    facing: Direction
}
impl Position {
    fn fix(self, map: &Cubemap) -> Position {
        fn rotate_coord((mut cr, mut cc): Coord, size: isize, di: Direction, df: Direction) -> Coord {
            (cr, cc) = (cr.rem_euclid(size), cc.rem_euclid(size));
            let last = size - 1;
            match (df as isize - di as isize).rem_euclid(4) {
                0 => (cr, cc),
                1 => (cc, last - cr),
                2 => (last - cr, last - cc),
                3 => (last - cc, cr),
                _ => unreachable!()
            }
        }

        let (pr, pc) = self.pos;
        let size = map.cubesize as isize;
        if (0..size).contains(&pr) && (0..size).contains(&pc) {
            self
        } else {
            let (cube, facing) = map.map[&self.cube][&self.facing];
            Position {
                cube, 
                pos: rotate_coord(self.pos, size, self.facing, facing),
                facing
            }
        }
    }

    fn password(self, map: &Cubemap) -> isize {
        let Position { cube, pos: (sx, sy), facing } = self;

        let (r, c) = map.chunks[cube];
        let pr = (r * map.cubesize) as isize + sx;
        let pc = (c * map.cubesize) as isize + sy;

        let fd = facing as isize;

        1000 * (pr + 1) + 4 * (pc + 1) + fd
    }
}

#[derive(Debug)]
struct Cubemap {
    chunks: Vec<CubeCoords>,
    cubesize: usize,
    map: HashMap<usize, HashMap<Direction, (usize, Direction)>>
}
impl Cubemap {
    fn new(chunks: Vec<CubeCoords>, cubesize: usize) -> Self {
        macro_rules! map {
            () => { HashMap::new() };
            ($($k:expr => $v:expr),+) => {{
                let mut m = HashMap::new();
                $(m.insert($k, $v);)+
                m
            }};
        }
        let mut map = match chunks.as_slice() {
            // test data
            [(0, 2), (1, 0), (1, 1), (1, 2), (2, 2), (2, 3)] => map! {
                0 => map! {
                    Direction::Right => (5, Direction::Left),
                    Direction::Down => (3, Direction::Down),
                    Direction::Left => (2, Direction::Down),
                    Direction::Up => (1, Direction::Down)
                },
                1 => map! {
                    Direction::Right => (2, Direction::Right),
                    Direction::Down => (4, Direction::Up),
                    Direction::Left => (5, Direction::Up)
                    // Direction::Up => {},
                },
                2 => map! {
                    Direction::Right => (3, Direction::Right),
                    Direction::Down => (4, Direction::Right)
                    // Direction::Left => {},
                    // Direction::Up => {}
                },
                3 => map! {
                    Direction::Right => (5, Direction::Down),
                    Direction::Down => (4, Direction::Down)
                    // Direction::Left => {},
                    // Direction::Up => {}
                },
                4 => map! {
                    Direction::Right => (5, Direction::Right)
                    // Direction::Down => {},
                    // Direction::Left => {},
                    // Direction::Up => {}
                },
                5 => map! {
                    // Direction::Right => {},
                    // Direction::Down => {},
                    // Direction::Left => {},
                    // Direction::Up => {}
                }
            },
            // actual data
            [(0, 1), (0, 2), (1, 1), (2, 0), (2, 1), (3, 0)] => map! {
                0 => map! {
                    Direction::Right => (1, Direction::Right),
                    Direction::Down => (2, Direction::Down),
                    Direction::Left => (3, Direction::Right),
                    Direction::Up => (5, Direction::Right)
                },
                1 => map! {
                    Direction::Right => (4, Direction::Left),
                    Direction::Down => (2, Direction::Left),
                    // Direction::Left => (5, Direction::Up),
                    Direction::Up => (5, Direction::Up)
                },
                2 => map! {
                    // Direction::Right => (3, Direction::Right),
                    Direction::Down => (4, Direction::Down),
                    Direction::Left => (3, Direction::Down)
                    // Direction::Up => {}
                },
                3 => map! {
                    Direction::Right => (4, Direction::Right),
                    Direction::Down => (5, Direction::Down)
                    // Direction::Left => {},
                    // Direction::Up => {}
                },
                4 => map! {
                    // Direction::Right => (5, Direction::Right)
                    Direction::Down => (5, Direction::Left)
                    // Direction::Left => {},
                    // Direction::Up => {}
                },
                5 => map! {
                    // Direction::Right => {},
                    // Direction::Down => {},
                    // Direction::Left => {},
                    // Direction::Up => {}
                }
            },
            _ => unimplemented!()
        };

        let sourcedests = map.iter()
           .flat_map(|(s, h)| h.iter().map(|(sd, (d, dd))| (*s, *d, *sd, *dd)))
           .collect::<Vec<_>>();
        for (s, d, sd, dd) in sourcedests {
            let dest = map.entry(d).or_default().entry(dd.invert());
            match dest {
                Entry::Occupied(_) => panic!("Did not expect ({:?}, {:?}) to already be mapped", d, dd.invert()),
                Entry::Vacant(ent) => ent.insert((s, sd.invert())),
            };
        }

        Self {
            chunks,
            cubesize,
            map,
        }
    }
}
#[derive(Debug)]
struct Board {
    board: [Vec<Vec<Tile>>; 6],
    cubemap: Cubemap,
    pos: Position,
    // cubemap: Cubemap
}

impl Board {
    fn apply_move(&mut self, m: Move) {
        match m {
            Move::Forward(n) => {
                let c = Ray::new(self).take(n + 1)
                    .take_while(|&p| self.get(p) == Some(Tile::Walk))
                    .last()
                    .expect("At least the starting position should have been walk");
                
                self.pos = c;
            },
            Move::Left  => self.pos.facing = self.pos.facing.rotate_left(),
            Move::Right => self.pos.facing = self.pos.facing.rotate_right(),
        }
    }

    fn get(&self, p: Position) -> Option<Tile> {
        let Position { cube, pos: (r, c), facing: _ } = p;
        let r = usize::try_from(r).ok()?;
        let c = usize::try_from(c).ok()?;

        self.board[cube].get(r).and_then(|r| r.get(c)).copied()
    }
}
impl FromStr for Board {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut lines = s.lines();
        lines.next_back();
        lines.next_back();
        
        let board: Vec<Vec<_>> = lines.map(|line| {
            line.chars()
                .map(|c| match c {
                    ' ' => Tile::Out,
                    '.' => Tile::Walk,
                    '#' => Tile::Rock,
                    _ => unimplemented!()
                })
                .collect()
        }).collect();

        let cubesize = board.iter()
            .map(|r| r.iter().skip_while(|&&e| e == Tile::Out).take_while(|&&e| e != Tile::Out).count())
            .min()
            .unwrap();
            
        let (chunks, cubemap) = chunk(board, cubesize);
        let board = chunks.try_into().unwrap();
        println!("{cubemap:?}");

        Ok(Self {
            board,
            cubemap: Cubemap::new(cubemap, cubesize),
            pos: Position {
                cube: 0,
                pos: (0, 0),
                facing: Direction::Right,
            }
        })
    }
}

struct Ray<'a> {
    start: Position,
    board: &'a Board
}
impl<'a> Ray<'a> {
    fn new(board: &'a Board) -> Self {
        Self { start: board.pos, board }
    }

    fn forward(&self, p: Position) -> Position {
        let (sx, sy) = p.pos;
        let (dx, dy) = p.facing.delta();

        Position {
            cube: p.cube,
            pos: (sx + dx, sy + dy),
            facing: p.facing,
        }.fix(&self.board.cubemap)
    }
}
impl<'a> Iterator for Ray<'a> {
    type Item = Position;

    fn next(&mut self) -> Option<Self::Item> {
        let e = self.start;
        self.start = self.forward(e);
        println!("~{:?}, {:?}", self.start, self.board.get(self.start));
        Some(e)
    }
}

fn chunk(mut board: Vec<Vec<Tile>>, size: usize) -> (Vec<Vec<Vec<Tile>>>, Vec<CubeCoords>) {
    let mut chunks = vec![];
    let mut map = vec![];

    for (i, row) in board.chunks_mut(size).enumerate() {
        let mut chunk: Vec<Vec<_>>;
        let mut j = 0;
        while !row[0].is_empty() {
            chunk = vec![];
            for r in row.iter_mut() {
                chunk.push(r.drain(0..size).collect());
            }
            if chunk[0][0] != Tile::Out { 
                chunks.push(chunk);
                map.push((i, j));
            }
            j += 1;
        }
    }

    (chunks, map)
}