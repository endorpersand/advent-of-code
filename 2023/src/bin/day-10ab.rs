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
    let boundary = points.clone();
    let points: Vec<_> = points.into_iter()
        .filter(|&pt| grid[pt] != Tile::NS && grid[pt] != Tile::EW)
        .map(|(r, c)| (r as f64, c as f64))
        .collect();
    let poly = Polygon(points);
    let out = {
        (0..grid.0.len())
            .flat_map(|r| (0..grid.0[0].len()).map(move |c| (r, c)))
            .filter(|id| !boundary.contains(id))
            .filter(|end| {
                let vec = Vector {
                    start: (100000., 10000.), // high size and not perfect ratio to hit points
                    end: (end.0 as f64, end.1 as f64)
                };
                
                let intersects = poly.to_sides()
                    .filter(|side| vec.is_intersecting(side))
                    .count();

                intersects % 2 == 1
            })
            .count()
    };
    println!("{out:?}");
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
// https://stackoverflow.com/questions/217578/how-can-i-determine-whether-a-2d-point-is-within-a-polygon
#[derive(Clone, Copy, PartialEq)]
struct Vector {
    start: (f64, f64),
    end: (f64, f64)
}
impl Vector {
    fn is_intersecting(&self, other: &Vector) -> bool {
        let (v1x1, v1y1) = self.start;
        let (v1x2, v1y2) = self.end;
        let (v2x1, v2y1) = other.start;
        let (v2x2, v2y2) = other.end;

        let (a1, b1, c1, mut d1, a2, b2, c2, mut d2);

        // Convert vector 1 to a line (line 1) of infinite length.
        // We want the line in linear equation standard form: A*x + B*y + C = 0
        // See: http://en.wikipedia.org/wiki/Linear_equation
        a1 = v1y2 - v1y1;
        b1 = v1x1 - v1x2;
        c1 = (v1x2 * v1y1) - (v1x1 * v1y2);

        // Every point (x,y), that solves the equation above, is on the line,
        // every point that does not solve it, is not. The equation will have a
        // positive result if it is on one side of the line and a negative one 
        // if is on the other side of it. We insert (x1,y1) and (x2,y2) of vector
        // 2 into the equation above.
        d1 = (a1 * v2x1) + (b1 * v2y1) + c1;
        d2 = (a1 * v2x2) + (b1 * v2y2) + c1;

        // If d1 and d2 both have the same sign, they are both on the same side
        // of our line 1 and in that case no intersection is possible. Careful, 
        // 0 is a special case, that's why we don't test ">=" and "<=", 
        // but "<" and ">".
        if d1 > 0. && d2 > 0. { return false };
        if d1 < 0. && d2 < 0. { return false };

        // The fact that vector 2 intersected the infinite line 1 above doesn't 
        // mean it also intersects the vector 1. Vector 1 is only a subset of that
        // infinite line 1, so it may have intersected that line before the vector
        // started or after it ended. To know for sure, we have to repeat the
        // the same test the other way round. We start by calculating the 
        // infinite line 2 in linear equation standard form.
        a2 = v2y2 - v2y1;
        b2 = v2x1 - v2x2;
        c2 = (v2x2 * v2y1) - (v2x1 * v2y2);

        // Calculate d1 and d2 again, this time using points of vector 1.
        d1 = (a2 * v1x1) + (b2 * v1y1) + c2;
        d2 = (a2 * v1x2) + (b2 * v1y2) + c2;

        // Again, if both have the same sign (and neither one is 0),
        // no intersection is possible.
        if d1 > 0. && d2 > 0. { return false };
        if d1 < 0. && d2 < 0. { return false };

        // If we get here, only two possibilities are left. Either the two
        // vectors intersect in exactly one point or they are collinear, which
        // means they intersect in any number of points from zero to infinite.
        if (a1 * b2) - (a2 * b1) == 0. { panic!("vectors are colinear") };

        // If they are not collinear, they must intersect in exactly one point.
        true
    }
}
#[derive(Clone, PartialEq)]
struct Polygon(Vec<(f64, f64)>);
impl Polygon {
    fn to_sides(&self) -> impl Iterator<Item=Vector> + '_ {
        self.0.windows(2)
            .map(|s| Vector { start: s[0], end: s[1] })
            .chain([Vector { start: *self.0.last().unwrap(), end: *self.0.first().unwrap() }])
    }
}