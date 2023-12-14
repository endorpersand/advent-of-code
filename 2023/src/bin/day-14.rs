fn main() {
    let txt = std::fs::read_to_string("inputs/14.txt").unwrap();
    let mut grid = parse(&txt);
    grid.rotate_left();

    {
        // Part A
        let mut g = grid.clone();
        g.move_rocks();
        println!("{}", g.calc_load());
    }

    {
        // Part B
        let mut grids = vec![grid.clone()];
        let start = loop {
            let mut g = grids.last().unwrap().clone();
            g.cycle();

            match grids.iter().rposition(|g1| g1 == &g) {
                Some(s) => break s,
                None    => grids.push(g),
            }
        };
        
        let len = grids.len() - start;
        let sel = start + ((1_000_000_000 - start) % len);
        println!("{}", grids[sel].calc_load());
    }
}

// n x n grid
#[derive(Clone, PartialEq, Eq)]
struct Grid {
    buffer: Vec<u8>,
    size: usize
}
fn parse(file: &str) -> Grid {
    let size = file.find('\n').unwrap();
    let buf = file.replace('\n', "");

    Grid { buffer: buf.into_bytes(), size }
}

impl Grid {
    // moves rocks so that they are at the leftmost in each row
    fn move_rocks(&mut self) {
        for c in 0..self.size {
            let column = &mut self.buffer[(c * self.size)..((c + 1) * self.size)];
            for seg in column.split_mut(|&t| t == b'#') {
                seg.sort_by_key(|&k| !k);
            }
        }
    }
    // calculates load, with leftmost being the max, rightmost being 1
    fn calc_load(&self) -> usize {
        (0..self.size)
            .flat_map(|c| {
                self.buffer[(c * self.size)..((c + 1) * self.size)]
                    .iter()
                    .rev()
                    .enumerate()
                    .filter(|(_, &tile)| tile == b'O')
                    .map(|(i, _)| i + 1)
            })
            .sum()
    }

    fn transpose(&mut self) {
        self.buffer = (0..self.size)
            .flat_map(|i| self.buffer[i..].iter().copied().step_by(self.size))
            .collect();
    }
    fn flip_major(&mut self) {
        for c in 0..self.size {
            self.buffer[(c * self.size)..((c + 1) * self.size)].reverse();
        }
    }
    fn rotate_left(&mut self) {
        self.flip_major();
        self.transpose();
    }
    fn rotate_right(&mut self) {
        self.transpose();
        self.flip_major();
    }
    fn cycle(&mut self) {
        for _ in 0..4 {
            self.move_rocks();
            self.rotate_right();
        }
    }

    #[allow(unused)]
    fn print(&self) {
        let buf = std::str::from_utf8(&self.buffer).unwrap();
        for c in 0..self.size {
            println!("{}", &buf[(c * self.size)..((c + 1) * self.size)]);
        }
        println!();
    }
}