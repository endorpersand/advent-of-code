fn main() {

    let txt = std::fs::read_to_string("inputs/16uu.txt").unwrap();
    let grid = Grid::parse(&txt);

    // PART A
    bench(|| {
        let out = State::new(&grid, (0, 0), Dir::Right).compute();
        println!("{out}");
    });

    // PART B
    bench(|| {
        let out = Iterator::chain(
            (0..grid.cols).flat_map(|c| [((0, c), Dir::Down),  ((grid.rows - 1, c), Dir::Up)]),
            (0..grid.rows).flat_map(|r| [((r, 0), Dir::Right), ((r, grid.cols - 1), Dir::Left)])
        )
            .map(|(i, d)| State::new(&grid, i, d).compute())
            .max()
            .unwrap();
        println!("{out}");
    });
}

fn bench(f: impl FnOnce()) {
    use std::time::Instant;

    println!("=== beginning computation ===");
    let start = Instant::now();
    f();
    let end = Instant::now();

    println!("=== complete. elapsed time: {:?} ===", end - start);
}

struct Grid {
    buffer: Vec<Tile>,
    cols: u32,
    rows: u32
}
impl Grid {
    fn parse(file: &str) -> Grid {
        let buffer: Vec<_> = file.bytes()
            .filter(|&s| s != b'\n')
            .map(|b| unsafe { std::mem::transmute::<u8, Tile>(b) })
            .collect();
        let cols = (file.find('\n').unwrap()) as u32;
        let rows = buffer.len() as u32 / cols;

        Grid { buffer, cols, rows }
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

type Coord = (u32, u32); // row col
struct State<'s> {
    grid: &'s Grid,
    energized: Vec<u8>, // hit locations
    tails: Vec<(Coord, Dir)> // beam pos, beam delta
}
impl<'s> State<'s> {
    fn new(grid: &'s Grid, tail_pos: Coord, tail_dir: Dir) -> Self {
        Self {
            grid,
            energized: vec![0; grid.buffer.len()],
            tails: vec![(tail_pos, tail_dir)],
        }
    }

    fn advance_tail(&mut self, (tr, tc): Coord, tail_dir: Dir) {
        let (nr, nc) = match tail_dir {
            Dir::Up    => (tr.wrapping_sub(1), tc),
            Dir::Right => (tr, tc.wrapping_add(1)),
            Dir::Down  => (tr.wrapping_add(1), tc),
            Dir::Left  => (tr, tc.wrapping_sub(1)),
        };

        if (0..self.grid.rows).contains(&nr) && (0..self.grid.cols).contains(&nc) {
            self.tails.push(((nr, nc), tail_dir));
        }
    }
    fn compute(&mut self) -> usize {
        while let Some((pos @ (posr, posc), delta)) = self.tails.pop() {
            let id = (posr * self.grid.cols + posc) as usize;
            let entry = &mut self.energized[id];
            if *entry & (1 << delta as u8) == 0 {
                *entry |= 1 << delta as u8;

                match self.grid.buffer[id] {
                    Tile::FwdMirror => self.advance_tail(pos, delta.reflect_fwd()),
                    Tile::BwdMirror => self.advance_tail(pos, delta.reflect_bwd()),
                    Tile::VertSplit if (delta as u8) & 1 != 0 => {
                        self.advance_tail(pos, Dir::Up);
                        self.advance_tail(pos, Dir::Down);
                    },
                    Tile::HorizSplit if (delta as u8) & 1 == 0 => {
                        self.advance_tail(pos, Dir::Left);
                        self.advance_tail(pos, Dir::Right);
                    },
                    _ => self.advance_tail(pos, delta),

                }
            }
        }

        self.energized.iter()
            .filter(|&&i| i != 0)
            .count()
    }
}