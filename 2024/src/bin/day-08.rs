use std::collections::{HashMap, HashSet};

fn main() {
    let input = std::fs::read_to_string("inputs/08.txt").unwrap();
    soln1(&input);
}

type Position = (usize, usize);
type PosDelta = (isize, isize);
fn compute_delta((lr, lc): Position, (rr, rc): Position) -> PosDelta {
    (rr.wrapping_sub(lr) as isize, rc.wrapping_sub(lc) as isize)
}
fn ray_sequence(start: Position, (dr, dc): PosDelta) -> impl Iterator<Item = Position> {
    std::iter::successors(Some(start), move |&(r, c)| Some((r.wrapping_add_signed(dr), c.wrapping_add_signed(dc))))
}
fn in_grid((r, c): Position, n_rows: usize, n_cols: usize) -> bool {
    (0..n_rows).contains(&r) && (0..n_cols).contains(&c)
}
#[allow(dead_code)]
fn soln1(input: &str) {
    let grid: Vec<Vec<_>> = input.lines()
        .map(|s| s.as_bytes().to_vec())
        .collect();
    let n_rows = grid.len();
    let n_cols = grid[0].len();

    let mut groups: HashMap<_, _> = HashMap::new();
    for (r, row) in grid.iter().enumerate() {
        for (c, &el) in row.iter().enumerate() {
            if el != b'.' {
                groups.entry(el).or_insert_with(Vec::new).push((r, c));
            }
        }
    }

    let mut antinodes = HashSet::new();
    for coords in groups.values() {
        for &c1 in coords {
            for &c2 in coords {
                if c1 != c2 {
                    let (dr, dc) = compute_delta(c1, c2);
                    let antinode = (c2.0.wrapping_add_signed(dr), c2.1.wrapping_add_signed(dc));
                    if in_grid(antinode, n_rows, n_cols) {
                        antinodes.insert(antinode);
                    }
                }
            }
        }
    }
    println!("{}", antinodes.len());

    let mut antinodes = HashSet::new();
    for coords in groups.values() {
        for &c1 in coords {
            for &c2 in coords {
                if c1 != c2 {
                    let (dr, dc) = compute_delta(c1, c2);
                    Iterator::chain(
                        ray_sequence(c1, (dr, dc)).take_while(|&n| in_grid(n, n_rows, n_cols)),
                        ray_sequence(c1, (-dr, -dc)).take_while(|&n| in_grid(n, n_rows, n_cols))
                    )
                        .for_each(|n| { antinodes.insert(n); });
                }
            }
        }
    }
    println!("{}", antinodes.len());
}