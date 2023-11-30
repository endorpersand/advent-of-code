use std::collections::HashSet;
use std::fs;

fn main() {
    let input = fs::read_to_string("inputs/8.txt").unwrap();
    let m = Matrix::new(&input);
    println!("{:?}", m.visible_from_edge().len());

    let max = (0..m.rows)
        .map(|r| (0..m.cols).map(|c| m.scenic(r, c)).max().unwrap())
        .max().unwrap();
    println!("{max}")
}

struct Matrix {
    m: Vec<Vec<usize>>,
    rows: usize,
    cols: usize
}

struct MatrixCursor<'a> {
    m: &'a Matrix,
    current: Option<(usize /* row */, usize /* col */)>,
    delta: (isize /* row */, isize /* col */)
}

impl Matrix {
    fn new(string: &str) -> Matrix {
        let m = string.lines()
            .map(|line| {
                line.chars()
                    .map(|m| String::from(m).parse().unwrap())
                    .collect()
            })
            .collect();

        let rows = string.lines().count();
        let cols = string.lines().next().unwrap().len();
        Matrix { m, rows, cols }
    }

    fn cursor(&self, at: (usize, usize), delta: (isize, isize)) -> MatrixCursor {
        MatrixCursor { m: self, current: Some(at), delta }
    }

    fn get(&self, row: usize, col: usize) -> Option<usize> {
        self.m.get(row).and_then(|r| r.get(col)).copied()
    }

    fn visible_from_edge(&self) -> HashSet<(usize, usize)> {
        let mut all_visibles = HashSet::new();
        // check rows
        for ri in 0..self.rows {
            let start = (ri, 0);
            let end = (ri, self.cols - 1);
            // down
            let mut tallest = None;
            for (r, c) in self.cursor(start, (0, 1)) {
                let tree = self.get(r, c);
                if tallest < Some(tree) {
                    all_visibles.insert((r, c));
                    tallest.replace(tree);
                }
            }
            // up
            let mut tallest = None;
            for (r, c) in self.cursor(end, (0, -1)) {
                let tree = self.get(r, c);
                if tallest < Some(tree) {
                    all_visibles.insert((r, c));
                    tallest.replace(tree);
                }
            }

            all_visibles.insert(start);
            all_visibles.insert(end);
        }

        // check cols
        for rj in 0..self.cols {
            let start = (0, rj);
            let end = (self.rows - 1, rj);
            // right
            let mut tallest = None;
            for (r, c) in self.cursor(start, (1, 0)) {
                let tree = self.get(r, c);
                if tallest < Some(tree) {
                    all_visibles.insert((r, c));
                    tallest.replace(tree);
                }
            }
            // left
            let mut tallest = None;
            for (r, c) in self.cursor(end, (-1, 0)) {
                let tree = self.get(r, c);
                if tallest < Some(tree) {
                    all_visibles.insert((r, c));
                    tallest.replace(tree);
                }
            }

            all_visibles.insert(start);
            all_visibles.insert(end);
        }

        all_visibles
    }

    fn count_scenic_in(&self, mut it: impl Iterator<Item=(usize, usize)>) -> usize {
        let mut ct = 0;
        if let Some((r, c)) = it.next() {
            let tree = self.get(r, c);
            for (r, c) in it {
                ct += 1;
                if self.get(r, c) >= tree {
                    break;
                }
            }
        }
        ct
    }

    fn scenic(&self, row: usize, col: usize) -> usize {
        [
            self.count_scenic_in(self.cursor((row, col), (0, -1))),
            self.count_scenic_in(self.cursor((row, col), (0, 1))),
            self.count_scenic_in(self.cursor((row, col), (-1, 0))),
            self.count_scenic_in(self.cursor((row, col), (1, 0))),
        ]
            .into_iter()
            .product()
    }
}

impl MatrixCursor<'_> {
    fn advance_cursor(&self, (row, col): (usize, usize)) -> Option<(usize, usize)> {
        #[inline]
        fn checked_add(a: usize, b: isize) -> Option<usize> {
            if b < 0 { 
                a.checked_sub(b.unsigned_abs()) 
            } else {
                a.checked_add(b as usize)
            }
        }

        let (dr, dc) = self.delta;

        let nr = checked_add(row, dr)?;
        let nc = checked_add(col, dc)?;
        
        (nr < self.m.rows && nc < self.m.cols)
            .then_some((nr, nc))
    }
}
impl Iterator for MatrixCursor<'_> {
    type Item = (usize, usize);

    fn next(&mut self) -> Option<Self::Item> {
        let cursor = self.current?;
        self.current = self.advance_cursor(cursor);
        
        Some(cursor)
    }
}