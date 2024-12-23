type Position = (usize, usize);
type PosDelta = (isize, isize);

const N: usize = 141;
struct ByteGrid<'a> {
    buf: &'a [u8]
}
impl ByteGrid<'_> {
    fn height(&self) -> usize {
        self.buf.len() % (N + 1)
    }
    fn get(&self, (r, c): Position) -> Option<u8> {
        ((0..N).contains(&r) && (0..self.height()).contains(&c))
            .then(|| self.buf.get(r.wrapping_mul(N + 1).wrapping_add(c)))?
            .copied()
    }
}

#[derive(Debug)]
struct Grid<T> {
    grid: Vec<[T; N]>
}
impl<T> Grid<T> {
    fn get(&self, (r, c): Position) -> Option<&T> {
        self.grid.get(r)?.get(c)
    }
}

fn translate((r, c): Position, (dr, dc): PosDelta) -> Position {
    (r.wrapping_add_signed(dr), c.wrapping_add_signed(dc))
}

fn half_ring(n: usize) -> impl Iterator<Item=PosDelta> {
    let n = n as isize;
    (0..n).flat_map(move |i| [(i, n - i), (n - i, -i)])
}
/// Takes a list of positions (sorted by distance from start) 
/// and produces an iterator of time saved.
fn cheat_iter<'a>(spaces: &'a [Position], rev_spaces: &'a Grid<isize>, n: usize) -> impl Iterator<Item=usize> + 'a {
    spaces.iter().enumerate()
        .flat_map(move |(i, &pi)| {
            half_ring(n)
                .map(move |d| translate(pi, d))
                .filter_map(|np| rev_spaces.get(np).filter(|&&v| !v.is_negative()))
                .map(move |&j| i.abs_diff(j as usize) - n)
        })
}

fn parse(input: &str) -> (ByteGrid<'_>, Position, Position) {
    let si = input.bytes().position(|b| b == b'S').unwrap();
    let start = (si / (N + 1), si % (N + 1));
    let ei = input.bytes().position(|b| b == b'E').unwrap();
    let end = (ei / (N + 1), ei % (N + 1));

    (ByteGrid { buf: input.as_bytes() }, start, end)
}
fn path(grid: &ByteGrid<'_>, start: Position, end: Position) -> (Vec<Position>, Grid<isize>) {
    /// Finds the next tile to traverse from `last` (and the direction to move).
    fn find_next(deltas: impl IntoIterator<Item=PosDelta>, grid: &ByteGrid<'_>, last: Position) -> (PosDelta, Position) {
        deltas.into_iter()
            .map(|d| (d, translate(last, d)))
            .find(|&(_, p)| grid.get(p).is_some_and(|w| w != b'#'))
            .unwrap()
    }

    // Compute the second node (and first direction of movement):
    let mut spaces = vec![start];
    let (mut dir, mut last) = find_next([(0, 1), (1, 0), (0, -1), (-1, 0)], grid, start);
    spaces.push(last);
    while last != end {
        let (dr, dc) = dir;
        let next @ (_, np) = find_next([dir, (dc, dr), (-dc, -dr)], grid, last);
        
        spaces.push(np);
        (dir, last) = next;
    }

    let mut rev_spaces = Grid { grid: vec![[-1; N]; grid.height()] };
    for (i, &(r, c)) in spaces.iter().enumerate() {
        rev_spaces.grid[r][c] = i as isize;
    }
    (spaces, rev_spaces)
}
pub fn part1(input: &str) -> usize {
    let (grid, start, end) = parse(input);
    let (spaces, rev_spaces) = path(&grid, start, end);

    cheat_iter(&spaces, &rev_spaces, 2)
        .filter(|&t| t >= 100) // limit distance, saves at least 100 ps of time
        .count()
}
pub fn part2(input: &str) -> usize {
    let (grid, start, end) = parse(input);
    let (spaces, rev_spaces) = path(&grid, start, end);

    (2..=20).flat_map(|n| cheat_iter(&spaces, &rev_spaces, n))
        .filter(|&t| t >= 100) // limit distance, saves at least 100 ps of time
        .count()
}

#[cfg(test)]
mod test {
    #[test]
    fn d20_correct() {
        let input = std::fs::read_to_string("inputs/20.txt").unwrap();
        assert_eq!(super::part1(&input), 1384);
        assert_eq!(super::part2(&input), 1008542);
    }
}