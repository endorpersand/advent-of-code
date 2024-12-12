use std::collections::{HashSet, VecDeque};

type Position = (usize, usize);
type PosDelta = (isize, isize);
fn offset(grid: &[Vec<u8>], (r, c): Position, (dr, dc): PosDelta) -> Option<Position> {
    let np @ (nr, nc) = (r.wrapping_add_signed(dr), c.wrapping_add_signed(dc));
    ((0..grid.len()).contains(&nr) && (0..grid[0].len()).contains(&nc)).then_some(np)
}
fn find_neighbors(grid: &[Vec<u8>], (r, c): Position) -> impl Iterator<Item=Position> + '_ {
    const DIRS: [PosDelta; 4] = [(0, 1), (0, -1), (1, 0), (-1, 0)];
    DIRS.into_iter()
        .filter_map(move |delta| offset(grid, (r, c), delta))
}

#[derive(Default, Debug)]
struct Group {
    points: usize,
    // part 1
    inner_edges: usize,
    // part 2
    corners: usize
}
impl Group {
    fn area(&self) -> usize {
        self.points
    }
    fn perimeter(&self) -> usize {
        self.points * 4 - self.inner_edges
    }
}

// TODO: Make this contiguous
fn parse(input: &str) -> Vec<Vec<u8>> {
    input.lines()
        .map(|l| l.bytes().collect())
        .collect()

}
pub fn part1(input: &str) -> usize {
    let grid = parse(input);

    let mut unexplored: Vec<_> = (0..grid.len())
        .flat_map(|r| (0..grid[0].len()).map(move |c| (r, c)))
        .rev()
        .collect();
    let mut visited = HashSet::new();
    let mut frontier = VecDeque::new();
    let mut accum = 0;

    while let Some(start) = unexplored.pop() {
        visited.insert(start);
        frontier.clear();
        frontier.push_back(start);
        
        let mut group = Group::default();
        while let Some(p @ (r, c)) = frontier.pop_front() {
            let value = grid[r][c];
            let mut inners = 0;

            // For each neighbor (which is in the group),
            // track an inner-edge and add to frontier
            find_neighbors(&grid, p)
                .filter(|&(nr, nc)| grid[nr][nc] == value)
                .for_each(|np| {
                    inners += 1;
                    if visited.insert(np) {
                        frontier.push_back(np);
                    }
                });

            group.points += 1;
            group.inner_edges += inners;
        }

        accum += group.area() * group.perimeter();
        unexplored.retain(|p| !visited.contains(p));
    }

    accum
}

pub fn part2(input: &str) -> usize {
    let grid = parse(input);

    let mut unexplored: Vec<_> = (0..grid.len())
        .flat_map(|r| (0..grid[0].len()).map(move |c| (r, c)))
        .rev()
        .collect();
    let mut visited = HashSet::new();
    let mut frontier = VecDeque::new();
    let mut accum = 0;

    while let Some(start) = unexplored.pop() {
        visited.insert(start);
        frontier.clear();
        frontier.push_back(start);
        
        let mut group = Group::default();
        while let Some(p @ (r, c)) = frontier.pop_front() {
            let value = grid[r][c];
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

            group.points += 1;
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

        accum += group.area() * group.corners;
        unexplored.retain(|p| !visited.contains(p));
    }

    accum
}

#[cfg(test)]
mod test {
    #[test]
    fn d12_correct() {
        let input = std::fs::read_to_string("inputs/12.txt").unwrap();
        assert_eq!(super::part1(&input), 1363682);
        assert_eq!(super::part2(&input), 787680);
    }
}

