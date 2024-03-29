use itertools::Itertools;

fn main() {
    let txt = std::fs::read_to_string("inputs/13.txt").unwrap();
    let State { grids } = parse(&txt);

    let out = grids.iter()
        .map(|g| {
            g.check_horiz().unwrap_or(0) * 100 + g.check_vert().unwrap_or(0)
        })
        .sum::<usize>();
    println!("{out}");

    let out = grids.iter()
        .map(|g| {
            g.check_horiz2().unwrap_or(0) * 100 + g.check_vert2().unwrap_or(0)
        })
        .sum::<usize>();
    println!("{out}");
}

#[derive(Debug)]
struct State {
    grids: Vec<Grid>
}
fn parse(file: &str) -> State {
    let mut lines = file.lines().peekable();
    let mut grids = vec![];

    while lines.peek().is_some() {
        let grid = lines.by_ref()
            .take_while(|l| !l.is_empty())
            .collect::<Grid>();

        grids.push(grid);
    }

    State { grids }
}

#[derive(Debug)]
struct Grid {
    buffer: Vec<bool>,
    cols: usize
}
impl<'a> FromIterator<&'a str> for Grid {
    fn from_iter<T: IntoIterator<Item = &'a str>>(iter: T) -> Self {
        let mut it = iter.into_iter();
        let first = it.next().expect("nz grid");

        let cols = first.len();
        let buffer = first.bytes()
            .chain(it.flat_map(|line| line.bytes()))
            .map(|b| b == b'#')
            .collect();

        Self { buffer, cols }
    }
}

impl Grid {
    fn rows(&self) -> usize {
        self.buffer.len() / self.cols
    }
    fn index_row(&self, r: usize) -> &[bool] {
        &self.buffer[(r * self.cols) .. ((r + 1) * self.cols)]
    }
    fn check_horiz(&self) -> Option<usize> {
        (0usize..self.rows())
            .tuple_windows()
            .find(|&(a, b)| {
                std::iter::zip((0..=a).rev(), b..self.rows())
                    .all(|(x, y)| self.index_row(x) == self.index_row(y))
            })
            .map(|(_, b)| b)
    }

    fn index_col(&self, c: usize) -> Vec<bool> {
        self.buffer[c..].iter()
            .copied()
            .step_by(self.cols)
            .collect()
    }
    fn check_vert(&self) -> Option<usize> {
        (0usize..self.cols)
            .tuple_windows()
            .find(|&(a, b)| {
                std::iter::zip((0..=a).rev(), b..self.cols)
                    .all(|(x, y)| self.index_col(x) == self.index_col(y))
            })
            .map(|(_, b)| b)
    }
}

// PART B
fn into_u64(b: &[bool]) -> u64 {
    b.iter()
        .copied()
        .rev()
        .enumerate()
        .map(|(i, b)| (b as u64) << i)
        .fold(0, |acc, cv| acc | cv)
}
impl Grid {
    fn check_horiz2(&self) -> Option<usize> {
        (0usize..self.rows())
            .tuple_windows()
            .find(|&(a, b)| {
                let diffs = std::iter::zip((0..=a).rev(), b..self.rows())
                    .map(|(x, y)| {
                        let xx = into_u64(self.index_row(x));
                        let yy = into_u64(self.index_row(y));

                        (xx ^ yy).count_ones()
                    }).sum::<u32>();

                diffs == 1
            })
            .map(|(_, b)| b)
    }

    fn check_vert2(&self) -> Option<usize> {
        (0usize..self.cols)
            .tuple_windows()
            .find(|&(a, b)| {
                let diffs = std::iter::zip((0..=a).rev(), b..self.cols)
                    .map(|(x, y)| {
                        let xx = into_u64(&self.index_col(x));
                        let yy = into_u64(&self.index_col(y));

                        (xx ^ yy).count_ones()
                    }).sum::<u32>();

                diffs == 1
            })
            .map(|(_, b)| b)
    }
}