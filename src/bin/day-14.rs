use std::collections::{HashSet, BTreeMap};
use std::fmt::Display;
use std::fs;

fn main() {
    let input = fs::read_to_string("inputs/14.txt").unwrap();
    
    // println!("{:?}", LineIterator::new((0, 0), (100, 0)).collect::<Vec<_>>());
    let mut field = Field::new();
    for (c1, c2) in parse_pairs(&input) {
        field.add_line(c1, c2);
    }

    while let Some(pt) = field.next_sand_point((500, 0)) {
        field.add_sand(pt);
    }
    println!("{}", field.sand);

    while let Some(pt) = field.next_sand_point2((500, 0)) {
        field.add_sand(pt);
    }
    // field.add_sand(field.next_sand_point_from_top2().unwrap());
    println!("{}", field.sand);
    // println!("{field}");
}

type Coord = (isize, isize);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Immovable {
    Sand,
    Rock
}
#[derive(Debug)]
struct Field {
    // y, x
    scenery: BTreeMap<isize, BTreeMap<isize, Immovable>>,
    sand: usize
}

impl Field {
    fn new() -> Self {
        Self {
            scenery: BTreeMap::new(),
            sand: 0
        }
    }

    fn add_line(&mut self, c1: Coord, c2: Coord) {
        for t in LineIterator::new(c1, c2) {
            self.add_rock(t);
        }
    }

    fn bounds(&self) -> (Coord /* min */, Coord /* max */) {
        let mut yvals = self.scenery.keys();
        let &ymin = yvals.next().unwrap();
        let &ymax = yvals.last().unwrap();

        let xvals: HashSet<_> = self.scenery.values()
            .flat_map(|xs| xs.keys())
            .collect();

        let &&xmin = xvals.iter().min().unwrap();
        let &&xmax = xvals.iter().max().unwrap();

        ((xmin, ymin), (xmax, ymax))
    }

    fn add_rock(&mut self, (cx, cy): Coord) {
        self.scenery.entry(cy).or_insert(BTreeMap::new()).insert(cx, Immovable::Rock);
    }
    fn add_sand(&mut self, (cx, cy): Coord) {
        self.scenery.entry(cy).or_insert(BTreeMap::new()).insert(cx, Immovable::Sand);
        self.sand += 1;
    }

    fn get(&self, x: isize, y: isize) -> Option<Immovable> {
        self.scenery.get(&y).and_then(|s| s.get(&x)).copied()
    }

    fn next_sand_point(&self, (px, py): Coord) -> Option<Coord> {
        // find the surface
        let colly = self.scenery.range((py + 1)..)
            .filter_map(|(y, hs)| hs.contains_key(&px).then_some(*y))
            .next()?;
        
        // our current point is one above the surface
        // px, colly - 1

        if self.get(px - 1, colly).is_none() {
            // check left.
            self.next_sand_point((px - 1, colly))
        } else if self.get(px + 1, colly).is_none() {
            // check right
            self.next_sand_point((px + 1, colly))
        } else {
            // it's where the current point is
            Some((px, colly - 1))
        }
    }

    fn get2(&self, x: isize, y: isize) -> Option<Immovable> {
        if self.scenery.get(&y).is_none() && self.scenery.get(&(y - 2)).is_some() {
            Some(Immovable::Rock)
        } else {
            self.scenery.get(&y).and_then(|s| s.get(&x)).copied()
        }
    }

    fn next_sand_point2(&self, (px, py): Coord) -> Option<Coord> {
        if self.get2(px, py).is_some() { None? }

        // find the surface
        let mcolly = self.scenery.range((py + 1)..)
            .filter_map(|(y, hs)| hs.contains_key(&px).then_some(*y))
            .next();
        
        match mcolly {
            // proceed from part A
            Some(colly) => {
                if self.get2(px - 1, colly).is_none() {
                    // check left.
                    self.next_sand_point2((px - 1, colly))
                } else if self.get2(px + 1, colly).is_none() {
                    // check right
                    self.next_sand_point2((px + 1, colly))
                } else {
                    // it's where the current point is
                    Some((px, colly - 1))
                }
            },
            None => {
                let colly = {
                    self.scenery.iter()
                        .filter_map(|(y, hs)| hs.values().any(|&im| im == Immovable::Rock).then_some(y))
                        .last()
                        .unwrap() + 1
                };

                Some((px, colly))
            },
        }
    }
}

fn parse_pairs(input: &str) -> HashSet<(Coord, Coord)> {
    let mut pairs = HashSet::new();
    for line in input.lines() {
        let rock_line = Pairwise::new(line.split(" -> ")
            .map(|s| {
                let (a, b) = s.split_once(',').unwrap();
                (a.parse().unwrap(), b.parse().unwrap())
            }
        ));
        pairs.extend(rock_line)
    }

    pairs
}

struct Pairwise<I: Iterator> {
    i: I,
    last: Option<I::Item>
}

impl<I: Iterator> Pairwise<I> {
    fn new(mut i: I) -> Self {
        let last = i.next();
        Self { i, last }
    }
}

impl<I: Iterator> Iterator for Pairwise<I> 
    where I::Item: Clone
{
    type Item = (I::Item, I::Item);

    fn next(&mut self) -> Option<Self::Item> {
        let left = self.last.take();
        let right = self.i.next();

        self.last = right.clone();

        left.zip(right)
    }
}

struct LineIterator {
    start: Coord,
    end: Coord,
    delta: (isize, isize)
}

impl LineIterator {
    fn new(a: Coord, b: Coord) -> Self {
        let (start, end) = if a < b {
            (a, b)
        } else {
            (b, a)
        };
        
        let delta = (
            (end.0 - start.0).signum(),
            (end.1 - start.1).signum()
        );
        Self { start, end, delta }
    }
}

impl Iterator for LineIterator {
    type Item = Coord;

    fn next(&mut self) -> Option<Self::Item> {
        (self.start <= self.end).then(|| {
            let c;
            (self.start, c) = ((self.start.0 + self.delta.0, self.start.1 + self.delta.1), self.start);
            c
        })
    }
}

impl Display for Field {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ((xmin, ymin), (xmax, ymax)) = self.bounds();

        (ymin..=ymax)
            .map(|y| {
                self.scenery.get(&y)
                    .map_or(
                        " ".repeat((xmax - xmin + 1) as usize),
                        |hs| (xmin..=xmax).map(|x| {
                            match hs.get(&x) {
                                Some(Immovable::Rock) => '#',
                                Some(Immovable::Sand) => 'o',
                                None => ' ',
                            }
                        }).collect()
                    )
            })
            .map(|s| writeln!(f, "{s}"))
            .collect()
    }
}