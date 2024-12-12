use std::collections::{HashSet, VecDeque};

fn main() {
    let input = std::fs::read_to_string("inputs/12.txt").unwrap();
    // part1(&input);
    part2(&input);
}

type Position = (usize, usize);
type PosDelta = (isize, isize);
fn offset(grid: &[Vec<u8>], (r, c): Position, (dr, dc): PosDelta) -> Option<Position> {
    let (nr, nc) = (r.wrapping_add_signed(dr), c.wrapping_add_signed(dc));
    grid.get(nr)?.get(nc).map(|_| (nr, nc))
}
fn find_neighbors(grid: &[Vec<u8>], (r, c): Position) -> impl Iterator<Item=Position> + '_ {
    const DIRS: [PosDelta; 4] = [(0, 1), (0, -1), (1, 0), (-1, 0)];
    DIRS.into_iter()
        .filter_map(move |delta| offset(grid, (r, c), delta))
}

#[derive(Default, Debug)]
struct Group {
    value: char,
    
    points: Vec<(Position, u8)>,
    // part 2
    corners: usize
}
impl Group {
    fn area(&self) -> usize {
        self.points.len()
    }
    fn perimeter(&self) -> usize {
        self.points.len() * 4 - self.points.iter().map(|&(_, d)| usize::from(d)).sum::<usize>()
    }
}
#[allow(dead_code)]
fn part1(input: &str) {
    let grid: Vec<Vec<_>> = input.lines()
        .map(|l| l.bytes().collect())
        .collect();

    let mut unexplored: Vec<_> = (0..grid.len())
        .flat_map(|r| (0..grid[0].len()).map(move |c| (r, c)))
        .rev()
        .collect();
    let mut visited = HashSet::new();
    let mut groups = vec![];

    while let Some(start) = unexplored.pop() {
        visited.insert(start);
        let mut frontier = VecDeque::from_iter([start]);
        
        let mut group = Group::default();
        while let Some(p @ (r, c)) = frontier.pop_front() {
            let value = grid[r][c];
            group.value = char::from(value);
            let neighbors: Vec<_> = find_neighbors(&grid, p)
                .filter(|&(nr, nc)| grid[nr][nc] == value)
                .collect();

            group.points.push((p, neighbors.len() as u8));

            neighbors.into_iter().for_each(|np| {
                if visited.insert(np) {
                    frontier.push_back(np);
                }
            });
        }

        groups.push(group);
        unexplored.retain(|p| !visited.contains(p));
    }
    let p1: usize = groups.iter().map(|g| g.area() * g.perimeter()).sum();
    println!("{p1}");
}
#[allow(dead_code)]
fn part2(input: &str) {
    let grid: Vec<Vec<_>> = input.lines()
        .map(|l| l.bytes().collect())
        .collect();

    let mut unexplored: Vec<_> = (0..grid.len())
        .flat_map(|r| (0..grid[0].len()).map(move |c| (r, c)))
        .rev()
        .collect();
    let mut visited = HashSet::new();
    let mut groups = vec![];

    while let Some(start) = unexplored.pop() {
        visited.insert(start);
        let mut frontier = VecDeque::from_iter([start]);
        
        let mut group = Group::default();
        while let Some(p @ (r, c)) = frontier.pop_front() {
            let value = grid[r][c];
            group.value = char::from(value);
            let neighbors = [
                (-1, -1), // UL
                (-1,  0), // UC
                (-1,  1), // UR
                ( 0, -1), // CL
                ( 0,  1), // CR
                ( 1, -1), // BL
                ( 1,  0), // BC
                ( 1,  1), // BR
            ].map(|delta| offset(&grid, p, delta).filter(|&(nr, nc)| grid[nr][nc] == value));
            let [ul, uc, ur, cl, cr, bl, bc, br] = neighbors;

            group.points.push((p, 0));
            if uc.is_some() == cl.is_some() && (uc.is_none() || ul.is_none()) { group.corners += 1 };
            if uc.is_some() == cr.is_some() && (uc.is_none() || ur.is_none()) { group.corners += 1 };
            if bc.is_some() == cl.is_some() && (bc.is_none() || bl.is_none()) { group.corners += 1 };
            if bc.is_some() == cr.is_some() && (bc.is_none() || br.is_none()) { group.corners += 1 };

            [uc, cl, cr, bc].into_iter().flatten().for_each(|np| {
                if visited.insert(np) {
                    frontier.push_back(np);
                }
            });
        }

        groups.push(group);
        unexplored.retain(|p| !visited.contains(p));
    }
    // println!("{groups:?}");
    let p2: usize = groups.iter().map(|g| g.area() * g.corners).sum();
    println!("{p2}");
}