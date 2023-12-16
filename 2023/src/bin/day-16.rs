use std::collections::HashMap;
use std::num::Wrapping;

fn main() {
    let txt = std::fs::read_to_string("inputs/16.txt").unwrap();
    let grid = Grid::parse(&txt);

    // PART A
    let out = State::new(&grid, 0, Dir::Right)
        .calc_energized();
    println!("{out}");

    // PART B
    let rows = grid.buffer.len() / grid.cols;

    let out = Iterator::chain(
        (0..grid.cols).flat_map(|c| [(0, c, Dir::Down), (rows - 1, c, Dir::Up)]),
        (0..rows).flat_map(|r| [(r, 0, Dir::Right), (r, grid.cols - 1, Dir::Left)])
    )
        .map(|(r, c, d)| (r * grid.cols + c, d))
        .map(|(i, d)| State::new(&grid, i, d).calc_energized())
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
        [Self::Right, Self::Up, Self::Left, Self::Down][self as usize]
    }
    fn reflect_bwd(self) -> Self {
        [Self::Left, Self::Down, Self::Right, Self::Up][self as usize]
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

    fn iterate(&mut self) {
        let tails = std::mem::take(&mut self.tails);
        for (pos, delta) in tails {
            let entry = self.energized.entry(pos).or_default();
            if *entry & (1 << delta as u8) == 0 {
                *entry |= 1 << delta as u8;

                let advances = match self.grid.buffer[pos] {
                    Tile::FwdMirror  => vec![delta.reflect_fwd()],
                    Tile::BwdMirror  => vec![delta.reflect_bwd()],
                    Tile::VertSplit  if (delta as u8) % 2 == 0 => vec![delta],
                    Tile::VertSplit  => vec![Dir::Up, Dir::Down],
                    Tile::HorizSplit if (delta as u8) % 2 != 0 => vec![delta],
                    Tile::HorizSplit => vec![Dir::Left, Dir::Right],
                    Tile::None       => vec![delta],
                };
    
                self.tails.extend({
                    advances.into_iter()
                        .map(|d| (d, self.grid.shift_index(pos, d)))
                        .filter_map(|(d, msi)| Some((msi?, d)))
                });
            }
        }
    }

    fn calc_energized(mut self) -> usize {
        while !self.tails.is_empty() {
            self.iterate();
        }
        self.energized.len()
    }
}