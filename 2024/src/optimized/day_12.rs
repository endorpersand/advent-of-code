type Position = (usize, usize);
type PosDelta = (isize, isize);
fn offset<T>(grid: &Grid<T>, (r, c): Position, (dr, dc): PosDelta) -> Option<Position> {
    let np @ (nr, nc) = (r.wrapping_add_signed(dr), c.wrapping_add_signed(dc));
    ((0..grid.len()).contains(&nr) && (0..grid[0].len()).contains(&nc)).then_some(np)
}
fn find_neighbors<T>(grid: &Grid<T>, (r, c): Position) -> impl Iterator<Item=Position> + '_ {
    const DIRS: [PosDelta; 4] = [(0, 1), (0, -1), (1, 0), (-1, 0)];
    DIRS.into_iter()
        .filter_map(move |delta| offset(grid, (r, c), delta))
}

const N: usize = 140;
struct Grid<T> {
    inner: Vec<[T; N]>
}
impl<T: Copy> Grid<T> {
    fn new(rows: usize) -> Self where T: Default {
        let inner = std::iter::repeat_with(|| std::array::from_fn(|_| Default::default()))
            .take(rows)
            .collect();

        Self { inner }
    }
}
impl Grid<bool> {
    fn insert(&mut self, (r, c): Position) -> bool {
        !std::mem::replace(&mut self[r][c], true)
    }
    fn find_unexplored(&self) -> Option<Position> {
        self.inner.iter().enumerate()
            .find_map(|(r, row)| {
                row.iter().enumerate()
                    .find_map(|(c, b)| (!b).then_some((r, c)))
            })
    }
}
impl<T> std::ops::Deref for Grid<T> {
    type Target = [[T; N]];

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}
impl<T> std::ops::DerefMut for Grid<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
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
fn parse(input: &str) -> Grid<u8> {
    let inner = input.lines()
        .map(|l| l.bytes().collect::<Vec<_>>())
        .flat_map(<[_; N]>::try_from)
        .collect();

    Grid { inner }

}
pub fn part1(input: &str) -> usize {
    let grid = parse(input);
    let mut explored = Grid::new(grid.len());
    let mut frontier = vec![];
    let mut accum = 0;

    while let Some(start) = explored.find_unexplored() {
        explored.insert(start);
        frontier.clear();
        frontier.push(start);
        
        let mut group = Group::default();
        while let Some(p @ (r, c)) = frontier.pop() {
            let value = grid[r][c];
            let mut inners = 0;

            // For each neighbor (which is in the group),
            // track an inner-edge and add to frontier
            find_neighbors(&grid, p)
                .filter(|&(nr, nc)| grid[nr][nc] == value)
                .for_each(|np| {
                    inners += 1;
                    if explored.insert(np) {
                        frontier.push(np);
                    }
                });

            group.points += 1;
            group.inner_edges += inners;
        }

        accum += group.area() * group.perimeter();
    }

    accum
}

pub fn part2(input: &str) -> usize {
    let grid = parse(input);
    let mut explored = Grid::new(grid.len());
    let mut frontier = vec![];
    let mut accum = 0;

    while let Some(start) = explored.find_unexplored() {
        explored.insert(start);
        frontier.clear();
        frontier.push(start);
        
        let mut group = Group::default();
        while let Some(p @ (r, c)) = frontier.pop() {
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
                if explored.insert(np) {
                    frontier.push(np);
                }
            });
        }

        accum += group.area() * group.corners;
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

