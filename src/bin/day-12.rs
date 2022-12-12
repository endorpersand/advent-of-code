use std::cmp::Reverse;
use std::collections::{BinaryHeap, HashMap};
use std::fs;

fn main() {
    let input = fs::read_to_string("inputs/12.txt").unwrap();

    const S: usize = 0;
    const E: usize = 27;
    let field: Vec<Vec<_>> = input.lines()
        .map(|line| {
            line.chars()
                .map(|c| match c {
                    'S' => S,
                    'E' => E,
                    c => (c as usize) - 96
                })
                .collect()
        })
        .collect();
    // println!("{field:?}");
    let start_y = field.iter().position(|row| row.contains(&S)).unwrap();
    let start_x = field[start_y].iter().position(|&e| e == S).unwrap();

    let end_y = field.iter().position(|row| row.contains(&E)).unwrap();
    let end_x = field[end_y].iter().position(|&e| e == E).unwrap();

    let mut frontier = Frontier::new(field.clone(), (start_x, start_y), (end_x, end_y));
    let path_len = frontier.find_path_len().expect("Could not find path");
    println!("{:?}", path_len);

    let (width, height) = (field[0].len(), field.len());
    let min_path = (0..width)
        .flat_map(|x| (0..height).map(move |y| (x, y)))
        .filter(|&(x, y)| field[y][x] <= 1)
        .map(|start| Frontier::new(field.clone(), start, (end_x, end_y)))
        .flat_map(|mut f| f.find_path_len())
        .min();
    println!("{min_path:?}")
}

type Coord = (usize, usize);

struct Frontier {
    field: Vec<Vec<usize>>,
    frontier: BinaryHeap<Reverse<(usize, Coord)>>,
    traversed: HashMap<Coord, usize>,
    target: Coord
}

impl Frontier {
    fn new(field: Vec<Vec<usize>>, start: Coord, end: Coord) -> Self {
        let mut frontier = BinaryHeap::new();
        frontier.push(Reverse((0, start)));

        Self {
            field,
            frontier,
            traversed: HashMap::new(),
            target: end
        }
    }

    fn get(&self, x: usize, y: usize) -> usize {
        self.field[y][x]
    }

    fn neighbors(&self, (x, y): Coord) -> Vec<Coord> {
        let height = self.get(x, y);
        macro_rules! chk {
            ($a:ident + $b:expr) => { $a.checked_add($b) };
            ($a:ident - $b:expr) => { $a.checked_sub($b) };
            ($a:ident) => { Some($a) };
        }

        [
            (chk!(x), chk!(y - 1)),
            (chk!(x), chk!(y + 1)),
            (chk!(x - 1), chk!(y)),
            (chk!(x + 1), chk!(y)),
        ]
            .into_iter()
            .filter_map(|(a, b)| a.zip(b))
            .filter(|(a, b)| a < &self.field[0].len() && b < &self.field.len())
            .filter(move |&(a, b)| self.get(a, b) <= height + 1)
            .collect()
    }

    fn find_path_len(&mut self) -> Option<usize> {
        while let Some(Reverse((start_dist, pos))) = self.frontier.pop() {
            self.traversed.insert(pos, start_dist);
            if pos == self.target {
                return Some(start_dist);
            }

            let nbs = self.neighbors(pos);
            for nb in nbs {
                if let Some(traversed_sd) = self.traversed.get(&nb) {
                    if *traversed_sd <= start_dist + 1 {
                        continue;
                    }
                }
                self.traversed.insert(nb, start_dist + 1);
                self.frontier.push(Reverse((start_dist + 1, nb)));
            }
        }
        
        None
    }
}