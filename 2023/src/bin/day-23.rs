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

struct BitSet {
    bits: Vec<u64>
}
impl BitSet {
    fn new() -> Self {
        BitSet { bits: vec![] }
    }
    fn new_with_capacity(cap: usize) -> Self {
        if cap != 0 {
            BitSet { bits: vec![0; cap.next_multiple_of(64) >> 6] }
        } else {
            BitSet::new()
        }
    }
    fn contains(&self, n: usize) -> bool {
        (self.bits[n >> 6] & (1 << (n & 63))) != 0
    }
    fn set(&mut self, n: usize) {
        self.bits[n >> 6] |= 1 << (n & 63)
    }
    fn clear(&mut self, n: usize) {
        self.bits[n >> 6] &= !(1 << (n & 63))
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
        visited: &mut BitSet
    ) -> Option<usize> 
        where I: Iterator<Item=(usize, usize)>
    {
        if start != end {
            visited.set(start);
    
            let max = neighbors(start)
                .filter_map(|(n, d)| {
                    match visited.contains(n) {
                        false => Some(d + path_inner(n, end, neighbors, visited)?),
                        true  => None,
                    }
                })
                .max();
    
            visited.clear(start);
    
            max
        } else {
            Some(0)
        }
    }

    path_inner(start, end, &mut neighbors, &mut BitSet::new_with_capacity(end /* close enoughhh */))
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

        let mut visited = HashSet::new();
        let mut frontier = VecDeque::from_iter([(start, start, 0)]);

        while let Some((head, mut tail, mut dist)) = frontier.pop_front() {
            if tail == end {
                red_grid.insert(head, tail, dist);
                continue;
            }

            visited.clear();
            visited.insert(head);

            let mut neis: Vec<_> = grid.neighbors_b(tail)
                .filter(|n| !visited.contains(n))
                .collect();
            
            while let &[next] = &*neis {
                visited.insert(tail);
                tail = next;
                dist += 1;

                neis = grid.neighbors_b(tail)
                    .filter(|n| !visited.contains(n))
                    .collect();
            }

            if red_grid.insert(head, tail, dist) {
                frontier.extend({
                    neis.into_iter()
                        .map(|n| (tail, n, 1))
                });
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