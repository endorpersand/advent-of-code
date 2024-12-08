use std::collections::{HashMap, HashSet};
use std::num::NonZeroU8;

fn main() {
    let input = std::fs::read_to_string("inputs/08.txt").unwrap();
    soln1(&input);
}

type Position = (usize, usize);
fn compute_antinode((lr, lc): Position, (rr, rc): Position) -> Position {
    (rr.wrapping_add(rr).wrapping_sub(lr), rc.wrapping_add(rc).wrapping_sub(lc))
}
fn arithmetic((mut sr, mut sc): Position, (dr, dc): (isize, isize)) -> impl Iterator<Item = Position> {
    std::iter::repeat_with(move || {
        let (r, c) = (sr, sc);
        sr = sr.wrapping_add_signed(dr);
        sc = sc.wrapping_add_signed(dc);
        (r, c)
    })
}
#[allow(dead_code)]
fn soln1(input: &str) {
    let grid: Vec<Vec<_>> = input.lines()
        .map(|s| {
            s.bytes()
                .map(|e| if e != b'.' { e } else { 0 })
                .map(NonZeroU8::new)
                .collect()
        })
        .collect();
    let n_rows = grid.len();
    let n_cols = grid[0].len();

    let mut groups: HashMap<_, _> = HashMap::new();
    for (r, row) in grid.iter().enumerate() {
        for (c, mel) in row.iter().enumerate() {
            if let &Some(el) = mel {
                groups.entry(el).or_insert_with(Vec::new).push((r, c));
            }
        }
    }

    let mut antinodes = HashSet::new();
    for coords in groups.values() {
        for &c1 in coords {
            for &c2 in coords {
                if c1 != c2 {
                    let antinode @ (ar, ac) = compute_antinode(c1, c2);
                    if (0..n_rows).contains(&ar) && (0..n_cols).contains(&ac) {
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
                    let dr = c2.0.wrapping_sub(c1.0) as isize;
                    let dc = c2.1.wrapping_sub(c1.1) as isize;
                    arithmetic(c1, (dr, dc))
                        .take_while(|(ar, ac)| (0..n_rows).contains(&ar) && (0..n_cols).contains(&ac))
                        .for_each(|antinode| { antinodes.insert(antinode); });
                    arithmetic(c1, (-dr, -dc))
                        .take_while(|(ar, ac)| (0..n_rows).contains(&ar) && (0..n_cols).contains(&ac))
                        .for_each(|antinode| { antinodes.insert(antinode); });
                }
            }
        }
    }
    println!("{}", antinodes.len());
}