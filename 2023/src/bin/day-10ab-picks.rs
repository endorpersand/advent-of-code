// This solution uses shoelace formula and Pick's theorem
// for part B.

use std::collections::HashMap;

use once_cell::sync::Lazy;

fn main() {
    let txt = std::fs::read_to_string("inputs/10.txt").unwrap();
    let State { grid } = parse(&txt);
    
    let start_coord = grid.0.iter()
        .enumerate()
        .find_map(|(r, row)| {
            row.iter()
                .enumerate()
                .find(|&(_, &tile)| tile == Tile::Start)
                .map(move |(c, _)| (r, c))
        })
        .unwrap();
    let mut points = vec![start_coord];

    // pick first direction that points to S's tile:
    let mut current_dir = [Direction::North, Direction::South, Direction::East, Direction::West]
        .into_iter()
        .find(|&d| get_neighbor(&grid, start_coord, d).is_some_and(|tile| tile.connects_to(d.invert())))
        .unwrap();
    let mut current_pt = current_dir.shift(start_coord).unwrap();
    
    while current_pt != start_coord {
        points.push(current_pt);
        current_dir = grid[current_pt].follow(current_dir.invert()).unwrap();
        current_pt = current_dir.shift(current_pt).unwrap();
    }
    println!("{:?}", points.len() / 2);

    // PART B
    println!("{}", shoelace_picks(&points));
}


#[derive(Debug)]
struct State {
    grid: Grid
}
fn parse(file: &str) -> State {
    let tiles = file.lines()
        .map(|t| t.bytes()
            .map(|b| match b {
                b'|' => Tile::NS,
                b'-' => Tile::EW,
                b'L' => Tile::NE,
                b'J' => Tile::NW,
                b'7' => Tile::SW,
                b'F' => Tile::SE,
                b'S' => Tile::Start,
                b'.' => Tile::None,
                s => unreachable!("{} invalid character", char::from(s))
            }).collect())
        .collect();

    State { grid: Grid(tiles) }
}
#[derive(Debug)]
struct Grid(Vec<Vec<Tile>>);
impl Grid {
    fn get(&self, idx: (usize, usize)) -> Option<&Tile> {
        let (r, c) = idx;
        self.0.get(r)?.get(c)
    }
}
impl std::ops::Index<(usize, usize)> for Grid {
    type Output = Tile;

    fn index(&self, (r, c): (usize, usize)) -> &Self::Output {
        &self.0[r][c]
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
#[repr(u8)]
enum Direction {
    North = 1 << 0,
    South = 1 << 1, 
    East  = 1 << 2, 
    West  = 1 << 3
}
impl Direction {
    fn from_id(id: u8) -> Self {
        match id {
            1 => Self::North,
            2 => Self::South,
            4 => Self::East,
            8 => Self::West,
            _ => panic!("Invalid id")
        }
    }
    fn invert(self) -> Self {
        match self {
            Direction::North => Self::South,
            Direction::South => Self::North,
            Direction::East => Self::West,
            Direction::West => Self::East,
        }
    }
    fn shift(self, (r, c): (usize, usize)) -> Option<(usize, usize)> {
        static DIR_MAP: Lazy<HashMap<Direction, (isize, isize)>> = Lazy::new(|| {
            let mut m = HashMap::new();
            m.insert(Direction::North, (-1, 0));
            m.insert(Direction::South, (1, 0));
            m.insert(Direction::East,  (0, 1));
            m.insert(Direction::West,  (0, -1));
            m
        });
    
        let (dr, dc) = DIR_MAP[&self];
        
        r.checked_add_signed(dr)
            .zip(c.checked_add_signed(dc))
    }
}
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[repr(u8)]
enum Tile {
    NS    = 0b00011,
    EW    = 0b01100,
    NE    = 0b00101,
    NW    = 0b01001,
    SW    = 0b01010,
    SE    = 0b00110,
    Start = 0b10000,
    None  = 0b00000,
}
impl Tile {
    fn connects_to(self, d: Direction) -> bool {
        (self as u8) & (d as u8) != 0
    }

    // If tile is NW and came from north, then move to west
    fn follow(self, d: Direction) -> Option<Direction> {
        self.connects_to(d).then(|| Direction::from_id((self as u8) ^ (d as u8)))
    }
}

fn get_neighbor(grid: &Grid, center: (usize, usize), d: Direction) -> Option<&Tile> {
    let idx = d.shift(center)?;
    grid.get(idx)
}

// PART B
fn shoelace_picks(boundary: &[(usize, usize)]) -> usize {
    // shoelace
    let &[first, .., last] = boundary else { panic!("expected 2 pts in boundary") };

    let doubled_area = boundary.windows(2)
        .map(|s| [s[0], s[1]])
        .chain(Some([last, first]))
        .map(|[(r1, c1), (r2, c2)]| [(r1 as isize, c1 as isize), (r2 as isize, c2 as isize)])
        .map(|[(r1, c1), (r2, c2)]| r1 * c2 - c1 * r2)
        .sum::<isize>()
        .unsigned_abs();

    assert_eq!(doubled_area % 2, 0);

    // by picks,
    // A = (inner points) + (boundary points / 2) - 1
    // Thus, (inner points) = A - (boundary points / 2) + 1
    doubled_area / 2 - boundary.len() / 2 + 1
}