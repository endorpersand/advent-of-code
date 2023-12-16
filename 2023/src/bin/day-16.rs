use std::collections::HashMap;
use std::num::Wrapping;

fn main() {
    let txt = std::fs::read_to_string("inputs/16.txt").unwrap();
    let grid = Grid::parse(&txt);

    // PART A
    let out = State::new(&grid, 0, Dir::Right).compute();
    println!("{out}");

    // PART B
    let rows = grid.buffer.len() / grid.cols;

    let out = Iterator::chain(
        (0..grid.cols).flat_map(|c| [(0, c, Dir::Down), (rows - 1, c, Dir::Up)]),
        (0..rows).flat_map(|r| [(r, 0, Dir::Right), (r, grid.cols - 1, Dir::Left)])
    )
        .map(|(r, c, d)| (r * grid.cols + c, d))
        .map(|(i, d)| State::new(&grid, i, d).compute())
        .max();
    println!("{out:?}");
}

struct Grid {
    buffer: Vec<Tile>,
    cols: usize
}
impl Grid {
    fn parse(file: &str) -> Grid {
        let buffer = file.bytes()
            .filter(|&s| s != b'\n')
            .map(|b| unsafe { std::mem::transmute::<u8, Tile>(b) })
            .collect();
        let cols = file.find('\n').unwrap();
    
        Grid { buffer, cols }
    }

    fn shift_index(&self, i: usize, delta: Dir) -> Option<usize> {
        let (mut r, mut c) = (Wrapping(i / self.cols), Wrapping(i % self.cols));
        match delta {
            Dir::Up    => r -= 1,
            Dir::Down  => r += 1,
            Dir::Left  => c -= 1,
            Dir::Right => c += 1,
        }

        let rows = self.buffer.len() / self.cols;
        let r_in = (0..rows).contains(&r.0);
        let c_in = (0..self.cols).contains(&c.0);

        // This is not unnecessary because it prevents overflow.
        #[allow(clippy::unnecessary_lazy_evaluations)]
        (r_in && c_in).then(|| r.0 * self.cols + c.0)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
#[repr(u8)]
#[allow(dead_code)]
enum Tile {
    FwdMirror = b'/',
    BwdMirror = b'\\',
    VertSplit = b'|',
    HorizSplit = b'-',
    None = b'.'
}
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
#[repr(u8)]
enum Dir {
    Up = 0,
    Right = 1,
    Down = 2,
    Left = 3,
}
impl Dir {
    fn reflect_fwd(self) -> Self {
        unsafe { std::mem::transmute((self as u8) ^ 0b01) }
    }
    fn reflect_bwd(self) -> Self {
        unsafe { std::mem::transmute((self as u8) ^ 0b11) }
    }
}
struct State<'s> {
    grid: &'s Grid,
    energized: HashMap<usize, u8>, // hit locations
    tails: Vec<(usize, Dir)> // beam pos, beam delta
}
impl<'s> State<'s> {
    fn new(grid: &'s Grid, tail_pos: usize, tail_dir: Dir) -> Self {
        Self {
            grid,
            energized: Default::default(),
            tails: vec![(tail_pos, tail_dir)],
        }
    }

    fn advance_tail(&mut self, tail_pos: usize, tail_dir: Dir) {
        if let Some(shifted_pos) = self.grid.shift_index(tail_pos, tail_dir) {
            self.tails.push((shifted_pos, tail_dir));
        }
    }
    fn compute(&mut self) -> usize {
        while let Some((pos, delta)) = self.tails.pop() {
            let entry = self.energized.entry(pos).or_default();
            if *entry & (1 << delta as u8) == 0 {
                *entry |= 1 << delta as u8;

                match self.grid.buffer[pos] {
                    Tile::FwdMirror => self.advance_tail(pos, delta.reflect_fwd()),
                    Tile::BwdMirror => self.advance_tail(pos, delta.reflect_bwd()),
                    Tile::VertSplit if (delta as u8) % 2 != 0 => {
                        self.advance_tail(pos, Dir::Up);
                        self.advance_tail(pos, Dir::Down);
                    },
                    Tile::HorizSplit if (delta as u8) % 2 == 0 => {
                        self.advance_tail(pos, Dir::Left);
                        self.advance_tail(pos, Dir::Right);
                    },
                    _ => self.advance_tail(pos, delta),

                }
            }
        }

        self.energized.len()
    }
}