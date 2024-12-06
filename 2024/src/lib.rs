use std::collections::HashSet;

type Position = (usize, usize);
type Orientation = (isize, isize);
type State = (Position, Orientation);

const UP: Orientation = (-1, 0);
fn rotate((dr, dc): Orientation) -> Orientation {
    (dc, -dr)
}
fn path_iter(grid: &[Vec<bool>], start: State) -> impl Iterator<Item = State> + '_ {
    std::iter::successors(Some(start), |&((r, c), (dr, dc))| {
        let (nr, nc) = r.checked_add_signed(dr).zip(c.checked_add_signed(dc))?;
        let blocked = grid.get(nr).and_then(|r| r.get(nc))?;

        match blocked {
            true  => Some(((r, c), rotate((dr, dc)))),
            false => Some(((nr, nc), (dr, dc))),
        }
    })
}

struct Data {
    grid: Vec<Vec<bool>>,
    start: State
}
fn parse(input: &str) -> Data {
    let grid = input.lines()
        .map(|s| s.bytes().map(|b| b == b'#').collect())
        .collect();
    let start = input.lines()
        .enumerate()
        .find_map(|(i, line)| {
            let (j, _) = line.bytes().enumerate().find(|&(_, b)| b == b'^')?;
            Some(((i, j), UP))
        }).unwrap();

    Data { grid, start }
}
pub fn d6p1(input: &str) {
    let Data { grid, start } = parse(input);
    
    let visited: HashSet<_> = path_iter(&grid, start)
        .map(|(p, _)| p)
        .collect();
    let p1 = visited.len();
    println!("{p1}");
}
pub fn d6p2(input: &str) {
    let Data { grid, start } = parse(input);
    
    let visited: HashSet<_> = path_iter(&grid, start)
        .map(|(p, _)| p)
        .collect();

    let mut grid2 = grid.clone();
    let obs_ct = visited.into_iter()
        .filter(|&(r, c)| {
            // Filter to only the tiles that would lead to a loop
            grid2[r][c] = true;
            let mut frontier = HashSet::new();
            let result = path_iter(&grid2, start).any(|st| !frontier.insert(st));
            grid2[r][c] = false;
            result
        })
        .count();

    let p2 = obs_ct;
    println!("{p2}");
}