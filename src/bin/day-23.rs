use std::collections::{VecDeque, HashSet, HashMap};
use std::fmt::Write;
use std::fs;
use std::str::FromStr;

fn main() {
    let input = fs::read_to_string("inputs/23.txt").unwrap();

    let mut grid: Grid = input.parse::<Grid>().unwrap();

    // part A
    for _ in 0..10 {
        grid.round();
    }
    println!("{}", grid);

    let empty: usize = grid.bool_grid()
        .into_iter()
        .map(|r| r.into_iter().filter(|x| !x).count())
        .sum();
    println!("{}", empty);

    // part B
    for i in 11.. {
        // we changed indexes isn't that funny
        let old_on = grid.on.clone();
        grid.round();

        if old_on == grid.on {
            println!("{i}");
            break;
        }
    }
}

type Coord = (isize, isize);
#[derive(Clone, Copy, PartialEq, Eq)]
enum Direction {
    North, South, West, East
}
impl Direction {
    fn main(self) -> Coord {
        match self {
            Direction::North => (-1,  0),
            Direction::South => ( 1,  0),
            Direction::West  => ( 0, -1),
            Direction::East  => ( 0,  1),
        }
    }

    fn delta(self) -> [Coord; 3] {
        let main @ (mr, mc) = self.main();

        let (ml, mr) = match self {
            Direction::North | Direction::South => ((mr, mc + 1), (mr, mc - 1)),
            Direction::East | Direction::West   => ((mr + 1, mc), (mr - 1, mc)),
        };
        
        [main, ml, mr]
    }
}
struct Grid {
    on: HashSet<Coord>,
    options: VecDeque<Direction>,
    new_pos: HashMap<Coord, Vec<Coord>>, /* the dest, all coordinates that asked */
}

fn neighbors((cx, cy): Coord) -> [Coord; 8] {
    [
        (cx - 1, cy - 1), (cx + 0, cy - 1), (cx + 1, cy - 1),
        (cx - 1, cy + 0), /*~~~~~~~~~~~~~*/ (cx + 1, cy + 0),
        (cx - 1, cy + 1), (cx + 0, cy + 1), (cx + 1, cy + 1),
    ]
}
fn target_neighbors((cx, cy): Coord, d: Direction) -> impl Iterator<Item=Coord> {
    d.delta().into_iter()
        .map(move |(dx, dy)| (cx + dx, cy + dy))
}

impl Grid {
    fn new(on: HashSet<Coord>) -> Self {
        Grid {
            on,
            options: [
                Direction::North, 
                Direction::South, 
                Direction::West, 
                Direction::East
            ].into_iter().collect(),
            new_pos: HashMap::new(),
        }
    }

    fn is_free(&self, it: impl IntoIterator<Item=Coord>) -> bool {
        it.into_iter().all(|c| !self.on.contains(&c))
    }

    fn round(&mut self) {
        for &c in self.on.iter() {
            // no neighbors? don't move
            if self.is_free(neighbors(c)) {
                self.new_pos.entry(c).or_default().push(c);
            } else {
                let new_pos = self.options.iter()
                    // .inspect(|&&d| println!("checking {:?}", target_neighbors(c, d).collect::<Vec<_>>()))
                    .find_map(|&d| {
                        self.is_free(target_neighbors(c, d)).then(|| d.main())
                    })
                    .map(|d| (c.0 + d.0, c.1 + d.1))
                    .unwrap_or(c);
                // println!("new pos: {new_pos:?}");
                self.new_pos.entry(new_pos).or_default().push(c);
            }
        }
        
        self.on.clear();
        for (dest, orig) in self.new_pos.drain() {
            if orig.len() == 1 {
                // if only one elf wants to move, then let them
                self.on.insert(dest);
            } else {
                // if multiple want to, then they can't.
                self.on.extend(orig);
            }
        }

        self.options.rotate_left(1);
    }

    fn bool_grid(&self) -> Vec<Vec<bool>> {
        let &rmin = self.on.iter().map(|(r, _)| r).min().unwrap();
        let &rmax = self.on.iter().map(|(r, _)| r).max().unwrap();
        let &cmin = self.on.iter().map(|(_, c)| c).min().unwrap();
        let &cmax = self.on.iter().map(|(_, c)| c).max().unwrap();

        (rmin..=rmax).map(|r| {
            (cmin..=cmax).map(|c| (r, c))
                .map(|coord| self.on.contains(&coord))
                .collect()
        }).collect()
    }
}
impl FromStr for Grid {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let on = s.lines()
            .enumerate()
            .flat_map(|(r, row)| {
                row.chars()
                    .enumerate()
                    .filter_map(move |(c, e)| (e == '#').then_some((r as _, c as _)))
            })
            .collect();
        
        Ok(Grid::new(on))
    }
}
impl std::fmt::Display for Grid {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.bool_grid()
            .into_iter()
            .try_for_each(|r| {
                r.into_iter()
                    .map(|e| if e { '#' } else { '.' })
                    .try_for_each(|c| f.write_char(c))?;
                writeln!(f)
            })
    }
}