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
    println!("{:?}", board.pos);
    for m in moves {
        board.apply_move(m);
        println!("{:?}", board.pos);
    }
    println!("{:?}", board.pos.password());
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Direction {
    Left, Right, Up, Down
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
}
type Coord = (isize, isize);
const FACING_RIGHT: Coord = ( 0,  1);
const FACING_LEFT: Coord  = ( 0, -1);
const FACING_UP: Coord    = (-1,  0);
const FACING_DOWN: Coord  = ( 1,  0);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Position {
    pos: Coord,
    facing: Direction
}
impl Position {
    fn password(&self) -> usize {
        let (px, py) = self.pos;
        let fd = match self.facing {
            Direction::Right => 0,
            Direction::Down => 1,
            Direction::Left => 2,
            Direction::Up => 3,
        };

        1000 * (px as usize + 1) + 4 * (py as usize + 1) + fd
    }
}
#[derive(Debug)]
struct Board {
    board: Vec<Vec<Tile>>,
    pos: Position
}

impl Board {
    fn apply_move(&mut self, m: Move) {
        match m {
            Move::Forward(n) => {
                let c = Ray::new(self).take(n + 1)
                    .take_while(|&p| self.get(p) == Some(Tile::Walk))
                    .last()
                    .expect("At least the starting position should have been walk");
                
                self.pos.pos = c;
            },
            Move::Left  => self.pos.facing = self.pos.facing.rotate_left(),
            Move::Right => self.pos.facing = self.pos.facing.rotate_right(),
        }
    }

    fn get(&self, (r, c): Coord) -> Option<Tile> {
        let r = usize::try_from(r).ok()?;
        let c = usize::try_from(c).ok()?;

        self.board.get(r).and_then(|r| r.get(c)).copied()
    }
}
impl FromStr for Board {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut lines = s.lines();
        lines.next_back();
        lines.next_back();
        let mut board: Vec<Vec<_>> = lines.map(|line| {
            line.chars()
                .map(|c| match c {
                    ' ' => Tile::Out,
                    '.' => Tile::Walk,
                    '#' => Tile::Rock,
                    _ => unimplemented!()
                })
                .collect()
        }).collect();

        let mut oob = std::iter::repeat(Tile::Out);
        let rowlen = board.iter().map(|r| r.len()).max().unwrap();
        for r in board.iter_mut() {
            r.extend(oob.by_ref().take(rowlen - r.len()));
        }

        let j = board.first().unwrap().iter().position(|&t| t == Tile::Walk).unwrap();

        Ok(Self {
            board, 
            pos: Position {
                pos: (0, j as _),
                facing: Direction::Right
            }
        })
    }
}

struct Ray<'a> {
    start: Coord,
    dir: Direction,
    board: &'a Board
}
impl<'a> Ray<'a> {
    fn new(board: &'a Board) -> Self {
        Self { start: board.pos.pos, dir: board.pos.facing, board }
    }

    fn torus_forward(&self, (sx, sy): Coord) -> Coord {
        let (dx, dy) = self.dir.delta();

        let xwrap = self.board.board.len() as _;
        let ywrap = self.board.board[sx as usize].len() as _;
        (
            (sx + dx).rem_euclid(xwrap), 
            (sy + dy).rem_euclid(ywrap)
        )
    }
    fn next_coord(&self) -> Coord {
        let (mut sx, mut sy) = self.start;

        (sx, sy) = self.torus_forward((sx, sy));
        while let Some(Tile::Out) = self.board.get((sx, sy)) {
            (sx, sy) = self.torus_forward((sx, sy));
        }

        (sx, sy)
    }
}
impl<'a> Iterator for Ray<'a> {
    type Item = Coord;

    fn next(&mut self) -> Option<Self::Item> {
        let e = self.start;
        self.start = self.next_coord();
        // println!("{:?}, {:?}", self.start, self.board.get(self.start));
        Some(e)
    }
}