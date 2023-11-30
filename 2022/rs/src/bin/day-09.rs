use std::collections::HashSet;
use std::fs;

fn main() {
    let input = fs::read_to_string("inputs/9.txt").unwrap();

    let instructions: Vec<_> = input.lines()
        .map(|line| {
            let [d_str, ct_str]: [&str; 2] = line.split(' ')
                .collect::<Vec<_>>()
                .try_into()
                .unwrap();
            
            let d = match d_str {
                "L" => Dir::L,
                "R" => Dir::R,
                "U" => Dir::U,
                "D" => Dir::D,
                _ => panic!("no")
            };
            let ct = ct_str.parse::<usize>().unwrap();
            (d, ct)
        })
        .collect();

    // PART A
    let mut rope = Rope::<1>::new();
    let mut tails = HashSet::new();
    tails.insert(rope.tail[0]);

    for &(d, ct) in instructions.iter() {
        for _ in 0..ct {
            let tail = rope.move_head(d);
            tails.insert(tail);
        }
    }

    // println!("{:?}", tails);
    println!("{}", tails.len());

    // PART B
    let mut rope = Rope::<9>::new();
    let mut tails = HashSet::new();
    tails.insert(rope.tail[0]);

    for &(d, ct) in instructions.iter() {
        for _ in 0..ct {
            let tail = rope.move_head(d);
            tails.insert(tail);
        }
    }

    // println!("{:?}", tails);
    println!("{}", tails.len());
}

type Coord = (isize, isize);
fn update_tail((hx, hy): Coord, (tx, ty): Coord) -> Coord {
    let dx = hx - tx;
    let dy = hy - ty;

    if dx.abs() <= 1 && dy.abs() <= 1 {
        // adjacent
        (tx, ty)
    } else {
        (tx + dx.signum(), ty + dy.signum())
    }
}

#[derive(Clone, Copy)]
enum Dir { L, R, U, D }

impl Dir {
    fn shift((x, y): Coord, d: Dir) -> Coord {
        match d {
            Dir::L => (x - 1, y),
            Dir::R => (x + 1, y),
            Dir::U => (x, y + 1),
            Dir::D => (x, y - 1),
        }
    }
}
struct Rope<const N: usize> {
    head: Coord,
    tail: [Coord; N]
}

impl<const N: usize> Rope<N> {
    fn new() -> Self {
        Rope {
            head: (0, 0),
            tail: [(0, 0); N],
        }
    }

    // return the last tail
    fn move_head(&mut self, d: Dir) -> Coord {
        self.head = Dir::shift(self.head, d);
        
        self.tail[0] = update_tail(self.head, self.tail[0]);
        for i in 1..(self.tail.len()) {
            self.tail[i] = update_tail(self.tail[i - 1], self.tail[i]);
        }
        
        *self.tail.last().unwrap()
    }
}