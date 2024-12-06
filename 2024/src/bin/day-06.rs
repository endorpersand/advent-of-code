use std::collections::HashSet;

fn main() {
    let input = std::fs::read_to_string("inputs/06.txt").unwrap();
    soln1(&input);
}

#[allow(dead_code)]
fn soln1(input: &str) {
    const UP: (isize, isize) = (-1, 0);
    fn rotate((dr, dc): (isize, isize)) -> (isize, isize) {
        (dc, -dr)
    }
    fn guard_path(grid: &[Vec<bool>], mut guard: Option<((usize, usize), (isize, isize))>) -> impl Iterator<Item = ((usize, usize), (isize, isize))> + '_ {
        std::iter::from_fn(move || {
            let state @ ((r, c), (dr, dc)) = guard.take()?;

            'inner: {
                let Some((nr, nc)) = r.checked_add_signed(dr).zip(c.checked_add_signed(dc)) else { break 'inner };
                let Some(blocked) = grid.get(nr).and_then(|r| r.get(nc)) else { break 'inner };
                
                guard.replace(match blocked {
                    true  => ((r, c), rotate((dr, dc))),
                    false => ((nr, nc), (dr, dc)),
                });
            }
            Some(state)
        })
    }
    let grid: Vec<Vec<_>> = input.lines()
        .map(|s| s.bytes().map(|b| b == b'#').collect())
        .collect();
    let m_guard = input.lines()
        .enumerate()
        .find_map(|(i, line)| {
            let (j, _) = line.bytes().enumerate().find(|&(_, b)| b == b'^')?;
            Some(((i, j), UP))
        });

    let p1 = guard_path(&grid, m_guard)
        .map(|(p, _)| p)
        .collect::<HashSet<_>>()
        .len();
    println!("{p1}");

    let mut grid2 = grid.clone();
    let obs_ct = grid.iter().enumerate().flat_map(|(r, row)| {
        row.iter().enumerate()
            .filter_map(move |(c, col)| (!col).then_some((r, c)))
    })
        .filter(|&(r, c)| {
            grid2[r][c] = true;
            let result = 'inner: {
                let mut visited = HashSet::new();
                for st in guard_path(&grid2, m_guard) {
                    if !visited.insert(st) {
                        break 'inner true;
                    }
                }
                false
            };
            grid2[r][c] = false;
            result
        })
        .count();

    let p2 = obs_ct;
    println!("{p2}");
}