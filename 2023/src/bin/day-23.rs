use std::collections::hash_map::Entry;
use std::collections::{VecDeque, HashMap, HashSet};

fn main() {
    let txt = std::fs::read_to_string("inputs/23.txt").unwrap();
    let grid = Grid::parse(&txt);

    let start = grid.buffer.iter().position(|&b| b == b'.').unwrap();
    let end   = grid.buffer.iter().rposition(|&b| b == b'.').unwrap();

    // Part A
    println!("Part A");
    assert_eq!(2222, find_max_path_len(start, end, |t| { grid.neighbors_a(t).map(|t| (t, 1)) }));
    
    // Part B
    println!("Part B");
    let rgrid = ReducedGrid::from_grid(&grid, start, end);
    assert_eq!(6590, find_max_path_len(start, end, |t| { rgrid.neighbors(t) }));
}

#[derive(Debug)]
struct Grid<'s> {
    buffer: &'s [u8],
    cols: usize
}
impl<'s> Grid<'s> {
    fn parse(file: &'s str) -> Self {
        let buffer = file.as_bytes();
        let cols = file.find('\n').unwrap() + 1;

        Self { buffer, cols }
    }
}

fn find_max_path_len<I>(
    start: usize, 
    end: usize, 
    mut neighbors: impl FnMut(usize) -> I
) -> usize 
    where I: Iterator<Item=(usize, usize)>
{
    fn path_inner<I>(
        start: usize, 
        end: usize, 
        neighbors: &mut impl FnMut(usize) -> I,
        visited: &mut HashSet<usize>
    ) -> Option<usize> 
        where I: Iterator<Item=(usize, usize)>
    {
        if start != end {
            visited.insert(start);
    
            let max = neighbors(start)
                .filter_map(|(n, d)| {
                    match visited.contains(&n) {
                        false => Some(d + path_inner(n, end, neighbors, visited)?),
                        true  => None,
                    }
                })
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
impl Grid<'_> {
    fn neighbors_a(&self, id: usize) -> impl Iterator<Item=usize> + '_ {
        let dirs = [
            self.cols.wrapping_neg(),
            1,
            self.cols,
            1usize.wrapping_neg()
        ];

        let mut d = [0; 4];
        match self.buffer[id] {
            b'^' => d[0] = dirs[0],
            b'>' => d[0] = dirs[1],
            b'v' => d[0] = dirs[2],
            b'<' => d[0] = dirs[3],
            _    => d = dirs
        }

        d.into_iter()
            .take_while(|&d| d != 0)
            .filter_map(move |delta| {
                let nid = id.wrapping_add(delta);

                self.buffer.get(nid)
                    .is_some_and(|b| !matches!(b, b'\n' | b'#'))
                    .then_some(nid)
            })
    }
}

// PART B
impl Grid<'_> {
    fn neighbors_b(&self, id: usize) -> impl Iterator<Item=usize> + '_ {
        let dirs = [
            self.cols.wrapping_neg(),
            1,
            self.cols,
            1usize.wrapping_neg()
        ];
        
        dirs.into_iter()
            .filter_map(move |delta| {
                let nid = id.wrapping_add(delta);

                self.buffer.get(nid)
                    .is_some_and(|b| !matches!(b, b'\n' | b'#'))
                    .then_some(nid)
            })
    }
}

#[derive(Debug)]
struct ReducedGrid {
    edges: HashMap<usize, HashMap<usize, usize>>
}
impl ReducedGrid {
    fn from_grid(grid: &Grid, start: usize, end: usize) -> Self {
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
    fn insert(&mut self, head: usize, tail: usize, v: usize) -> bool {
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
    fn neighbors(&self, id: usize) -> impl Iterator<Item=(usize, usize)> + '_ {
        self.edges.get(&id)
            .into_iter()
            .flatten()
            .map(|(&k, &v)| (k, v))
    }
}