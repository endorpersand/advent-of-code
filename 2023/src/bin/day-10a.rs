// Solves 10a. See day-10ab for the revised version that also solves part b.

use std::collections::{HashMap, VecDeque};

use once_cell::sync::Lazy;

fn main() {
    let txt = std::fs::read_to_string("inputs/10.txt").unwrap();
    let State { tiles } = parse(&txt);
    
    let mut searched: HashMap<_, usize> = HashMap::new();
    
    let start_coord = tiles.iter()
        .enumerate()
        .find_map(|(r, row)| {
            row.iter()
                .enumerate()
                .find(|&(_, &tile)| tile == Tile::Start)
                .map(move |(c, _)| (r, c))
        })
        .unwrap();

    let mut frontier = VecDeque::from_iter([(start_coord, None::<(usize, usize)>)]);
    while let Some((tile, m_parent)) = frontier.pop_front() {
        #[allow(clippy::map_entry)]
        if !searched.contains_key(&tile) {
            searched.insert(tile, m_parent.map_or(0, |p| searched[&p] + 1));
            
            frontier.extend({
                neighbors_on_loop(&tiles, tile)
                    .into_iter()
                    .map(|(_, nei)| (nei, Some(tile)))
            });
        }
    }

    let (_, &max) = searched.iter()
        .max_by_key(|&(_, &id)| id)
        .unwrap();
    println!("{max}");

}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
enum Direction {
    North, South, East, West
}
impl Direction {
    fn invert(self) -> Self {
        match self {
            Direction::North => Self::South,
            Direction::South => Self::North,
            Direction::East => Self::West,
            Direction::West => Self::East,
        }
    }
}
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Tile {
    NS,
    EW,
    NE,
    NW,
    SW,
    SE,
    Start,
    None,
}
impl Tile {
    fn connects_to(self, d: Direction) -> bool {
        matches!((self, d), 
            | (Tile::NS, Direction::North) 
            | (Tile::NS, Direction::South) 
            | (Tile::EW, Direction::East) 
            | (Tile::EW, Direction::West) 
            | (Tile::NE, Direction::North) 
            | (Tile::NE, Direction::East) 
            | (Tile::NW, Direction::North) 
            | (Tile::NW, Direction::West) 
            | (Tile::SW, Direction::South) 
            | (Tile::SW, Direction::West) 
            | (Tile::SE, Direction::South) 
            | (Tile::SE, Direction::East))
    }
    fn split(self) -> Option<[Direction; 2]> {
        match self {
            Tile::NS => Some([Direction::North, Direction::South]),
            Tile::EW => Some([Direction::East, Direction::West]),
            Tile::NE => Some([Direction::North, Direction::East]),
            Tile::NW => Some([Direction::North, Direction::West]),
            Tile::SW => Some([Direction::South, Direction::West]),
            Tile::SE => Some([Direction::South, Direction::East]),
            Tile::Start => None,
            Tile::None => None,
        }
    }
}
#[derive(Debug)]
struct State {
    tiles: Vec<Vec<Tile>>
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

    State { tiles }
}

fn get_index<T>(vec: &[Vec<T>], (r, c): (usize, usize)) -> Option<&T> {
    vec.get(r)?.get(c)
}
static DIR_MAP: Lazy<HashMap<Direction, (isize, isize)>> = Lazy::new(|| {
    let mut m = HashMap::new();
    m.insert(Direction::North, (-1, 0));
    m.insert(Direction::South, (1, 0));
    m.insert(Direction::East,  (0, 1));
    m.insert(Direction::West,  (0, -1));
    m
});
fn neighbors_on_loop(vec: &[Vec<Tile>], (r, c): (usize, usize)) -> Vec<(Direction, (usize, usize))> {
    let possible_dirs = match vec[r][c] {
        Tile::Start => vec![Direction::North, Direction::South, Direction::East, Direction::West],
        Tile::None => vec![],
        tile => {
            tile.split()
                .map_or(vec![], |d| d.into_iter().collect())
        }
    };

    possible_dirs.into_iter()
        .filter_map(|d| {
            let (dr, dc) = DIR_MAP[&d];

            r.checked_add_signed(dr)
                .zip(c.checked_add_signed(dc))
                .map(|neighbor| (d, neighbor))
        })
        .filter(|&(d, idx)| {
            // assert idx is in bounds
            let Some(&tile) = get_index(vec, idx) else { return false };
            // assert tile could connect
            tile != Tile::None && tile.connects_to(d.invert())
        })
        .collect()
}