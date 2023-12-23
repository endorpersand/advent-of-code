use std::collections::VecDeque;

fn main() {
    let txt = std::fs::read_to_string("inputs/23.txt").unwrap();
    let grid = Grid::parse(&txt);

    let start = (0, grid.buffer[0..grid.cols].iter().position(|&b| b == b'.').unwrap());
    let end = (grid.rows - 1, grid.buffer[(grid.rows - 1) * grid.cols..].iter().position(|&b| b == b'.').unwrap());
    println!("{}", find_max_path_len(&grid, start, end))
}

#[derive(Debug)]
struct Grid {
    buffer: Vec<u8>,
    cols: usize,
    rows: usize
}
impl Grid {
    fn parse(file: &str) -> Self {
        let buffer: Vec<_> = file.bytes()
            .filter(|&b| b != b'\n')
            .collect();

        let cols = file.find('\n').unwrap();
        let rows = buffer.len() / cols;

        Self { buffer, cols, rows }
    }

    fn in_bounds(&self, (r, c): (usize, usize)) -> bool {
        (0..self.rows).contains(&r) && (0..self.cols).contains(&c)
    }

    fn index(&self, (r, c): (usize, usize)) -> u8 {
        self.buffer[r * self.cols + c]
    }

    fn neighbors(&self, (r, c): (usize, usize)) -> impl Iterator<Item=(usize, usize)> + '_ {
        [Dir::Up, Dir::Right, Dir::Down, Dir::Left]
            .into_iter()
            .filter_map(move |d| {
                let (dr, dc) = d.delta();
                let (nr, nc) = (r.wrapping_add_signed(dr), c.wrapping_add_signed(dc));
                (self.in_bounds((nr, nc)) && self.index((nr, nc)) != b'#').then_some((nr, nc))
            })
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
#[allow(unused)]
#[repr(u8)]
enum Dir {
    Up = 0, Right = 1, Down = 2, Left = 3
}
impl Dir {
    fn delta(self) -> (isize, isize) {
        match self {
            Dir::Up    => (-1,  0),
            Dir::Right => ( 0,  1),
            Dir::Down  => ( 1,  0),
            Dir::Left  => ( 0, -1),
        }
    }
}

fn find_max_path_len(grid: &Grid, start: (usize, usize), end: (usize, usize)) -> usize {
    #[derive(Debug, Default, Clone)]
    struct Path {
        path: Vec<(usize, usize)>,
        reached_end: bool
    }

    let mut paths = vec![Path::default()];
    let mut frontier = VecDeque::from_iter([(start, 0)]);

    while let Some((tile, path_id)) = frontier.pop_back() {
        if tile == end {
            paths[path_id].reached_end = true;
            continue;
        }
        
        let neis: Vec<_> = grid.neighbors(tile)
            .filter(|n| !paths[path_id].path.contains(n))
            .collect::<Vec<_>>();

        let Some((&first_nei, rest_neis)) = neis.split_first() else { continue };
        
        // register new paths for everyone else first
        for &nei in rest_neis {
            let new_path_id = paths.len();
            paths.push(paths[path_id].clone());
            frontier.push_back((nei, new_path_id));
            paths[new_path_id].path.push(nei);
        }

        // then go back to the original path
        frontier.push_back((first_nei, path_id));
        paths[path_id].path.push(first_nei);
    }

    paths.into_iter()
        .filter(|p| p.reached_end)
        .map(|p| p.path.len())
        .max()
        .unwrap()
}