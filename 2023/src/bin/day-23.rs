use std::collections::hash_map::Entry;
use std::collections::{VecDeque, HashMap, HashSet};

fn main() {
    let txt = std::fs::read_to_string("inputs/23.txt").unwrap();
    let grid = Grid::parse(&txt);

    let start = (0, grid.buffer[0..grid.cols].iter().position(|&b| b == b'.').unwrap());
    let end = (grid.rows - 1, grid.buffer[(grid.rows - 1) * grid.cols..].iter().position(|&b| b == b'.').unwrap());
    
    // Part A
    println!("Part A");
    assert_eq!(2222, find_max_path_len(start, end, |t| { grid.neighbors_a(t).map(|t| (t, 1)) }));
    
    // Part B
    println!("Part B");
    let rgrid = ReducedGrid::from_grid(&grid, start, end);
    assert_eq!(6590, find_max_path_len(start, end, |t| { rgrid.neighbors(t) }));
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


fn find_max_path_len<I>(
    start: (usize, usize), 
    end: (usize, usize), 
    mut neighbors: impl FnMut((usize, usize)) -> I
) -> usize 
    where I: Iterator<Item=((usize, usize), usize)>
{
    fn path_inner<I>(
        start: (usize, usize), 
        end: (usize, usize), 
        neighbors: &mut impl FnMut((usize, usize)) -> I,
        visited: &mut HashSet<(usize, usize)>
    ) -> Option<usize> 
        where I: Iterator<Item=((usize, usize), usize)>
    {
        if start != end {
            visited.insert(start);
    
            let neis: Vec<_> = neighbors(start)
                .filter(|(n, _)| !visited.contains(n))
                .collect();
    
            let max = neis.into_iter()
                .filter_map(|(n, d)| Some(d + path_inner(n, end, neighbors, visited)?))
                .max();
    
            visited.remove(&start);
    
            max
        } else {
            Some(0)
        }
    }

    path_inner(start, end, &mut neighbors, &mut HashSet::new())
        .unwrap()
}

// PART A
impl Grid {
    fn neighbors_a(&self, (r, c): (usize, usize)) -> impl Iterator<Item=(usize, usize)> {
        let vec = match self.index((r, c)) {
            b'>' => vec![Dir::Right],
            b'v' => vec![Dir::Down],
            b'<' => vec![Dir::Left],
            b'^' => vec![Dir::Up],
            _ => {
                [Dir::Up, Dir::Right, Dir::Down, Dir::Left]
                    .into_iter()
                    .filter(|d| {
                        let (dr, dc) = d.delta();
                        let (nr, nc) = (r.wrapping_add_signed(dr), c.wrapping_add_signed(dc));
                        self.in_bounds((nr, nc)) && self.index((nr, nc)) != b'#'
                    })
                    .collect()
            }
        };

        vec.into_iter()
            .map(move |d| {
                let (dr, dc) = d.delta();
                (r.wrapping_add_signed(dr), c.wrapping_add_signed(dc))
            })
    }
}

// PART B
impl Grid {
    fn neighbors_b(&self, (r, c): (usize, usize)) -> impl Iterator<Item=(usize, usize)> + '_ {
        [Dir::Up, Dir::Right, Dir::Down, Dir::Left]
            .into_iter()
            .filter_map(move |d| {
                let (dr, dc) = d.delta();
                let (nr, nc) = (r.wrapping_add_signed(dr), c.wrapping_add_signed(dc));
                (self.in_bounds((nr, nc)) && self.index((nr, nc)) != b'#').then_some((nr, nc))
            })
    }
}

#[derive(Debug)]
struct ReducedGrid {
    edges: HashMap<(usize, usize), HashMap<(usize, usize), usize>>
}
impl ReducedGrid {
    fn from_grid(grid: &Grid, start: (usize, usize), end: (usize, usize)) -> Self {
        let mut red_grid = ReducedGrid { edges: HashMap::new() };
        let mut frontier = VecDeque::from_iter([vec![start]]);
        while let Some(mut path) = frontier.pop_front() {
            let head = path[0];
            let &tail = path.last().unwrap();

            let neis: Vec<_> = grid.neighbors_b(tail)
                .filter(|n| !path.contains(n))
                .collect();

            match &*neis {
                [] => continue,
                &[next] if next == end => {
                    red_grid.insert(head, next, path.len());
                },
                &[next] => {
                    path.push(next);
                    frontier.push_back(path);
                },
                _ => {
                    if red_grid.insert(head, tail, path.len() - 1) {
                        frontier.extend({
                            neis.into_iter()
                                .map(|n| vec![tail, n])
                        });
                    }
                }
            }
        }

        red_grid
    }
    fn insert(&mut self, head: (usize, usize), tail: (usize, usize), v: usize) -> bool {
        let head_map = self.edges.entry(head).or_default();
        if let Entry::Vacant(e) = head_map.entry(tail) {
            e.insert(v);

            let tail_map = self.edges.entry(tail).or_default();
            if let Entry::Vacant(e) = tail_map.entry(head) {
                e.insert(v);

                return true;
            }
        }

        false
    }
    fn neighbors(&self, id: (usize, usize)) -> impl Iterator<Item=((usize, usize), usize)> + '_ {
        self.edges.get(&id)
            .into_iter()
            .flatten()
            .map(|(&k, &v)| (k, v))
    }
}