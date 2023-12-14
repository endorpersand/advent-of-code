fn main() {
    let txt = std::fs::read_to_string("inputs/14.txt").unwrap();
    let mut grid = parse(&txt);
    grid.transpose();

    {
        // Part A
        let mut g = grid.clone();
        g.move_rocks();
        println!("{}", g.calc_load());
    }

    {
        // Part B
        let mut grids = vec![grid.clone()];
        let mut cycle_start = None;
        for _ in 0..1_000_000_000 {
            let mut g = grids.last().unwrap().clone();
            g.cycle();
            if let Some(p) = grids.iter().rposition(|g1| g1 == &g) {
                cycle_start.replace(p);
                break;
            }
            grids.push(g);
        }

        let start = cycle_start.unwrap();
        let len = grids.len() - start;
        let sel = start + ((1_000_000_000 - start) % len);
        println!("{}", grids[sel].calc_load());
    }
}

#[derive(Clone, PartialEq, Eq)]
struct Grid {
    // column major
    buffer: Vec<u8>,
    cols: usize
}
fn parse(file: &str) -> Grid {
    let cols = file.find('\n').unwrap();
    let buf = file.replace('\n', "");

    Grid { buffer: buf.as_bytes().to_vec(), cols }
}

impl Grid {
    fn rows(&self) -> usize {
        self.buffer.len() / self.cols
    }

    fn move_rocks(&mut self) {
        let rows = self.rows();
        for c in 0..self.cols {
            let column = &mut self.buffer[(c * rows)..((c + 1) * rows)];
            for seg in column.split_mut(|&t| t == b'#') {
                seg.sort_by_key(|&k| !k);
            }
        }
    }
    fn calc_load(&self) -> usize {
        let rows = self.rows();
        (0..self.cols)
            .flat_map(|c| {
                self.buffer[(c * rows)..((c + 1) * rows)]
                    .iter()
                    .rev()
                    .enumerate()
                    .filter(|(_, &tile)| tile == b'O')
                    .map(|(i, _)| i + 1)
            })
            .sum()
    }

    fn transpose(&mut self) {
        self.buffer = (0..self.cols)
            .flat_map(|i| self.buffer[i..].iter().copied().step_by(self.cols))
            .collect();
    }
    fn flip_major(&mut self) {
        let rows = self.rows();
        for c in 0..self.cols {
            self.buffer[(c * rows)..((c + 1) * rows)].reverse();
        }
    }
    fn rotate(&mut self) {
        self.flip_major();
        self.transpose();
    }
    fn cycle(&mut self) {
        for _ in 0..4 {
            self.move_rocks();
            self.rotate();
        }
    }

    #[allow(unused)]
    fn print(&self) {
        let buf = std::str::from_utf8(&self.buffer).unwrap();
        let rows = self.rows();
        for c in 0..self.cols {
            println!("{}", &buf[(c * rows)..((c + 1) * rows)]);
        }
        println!();
    }
}