type Position = (usize, usize);
type PosDelta = (isize, isize);

#[derive(Debug)]
struct Grid<T> {
    grid: Vec<Vec<T>>
}
impl<T> Grid<T> {
    fn get(&self, (r, c): Position) -> Option<&T> {
        self.grid.get(r)?.get(c)
    }
}

fn translate((r, c): Position, (dr, dc): PosDelta) -> Position {
    (r.wrapping_add_signed(dr), c.wrapping_add_signed(dc))
}
fn manhattan((r0, c0): Position, (r1, c1): Position) -> usize {
    r1.abs_diff(r0) + c1.abs_diff(c0)
}
/// Takes a list of positions (sorted by distance from start) 
/// and produces an iterator of (point 0, point 1, manhattan distance between points, time saved) tuples.
fn cheat_iter(spaces: &[Position]) -> impl Iterator<Item=(usize, usize, usize, usize)> + '_ {
    spaces.iter().enumerate().flat_map(|(i, &pi)| {
        std::iter::zip((i + 1).., &spaces[(i + 1)..]).map(move |(j, &pj)| {
            let dist = manhattan(pi, pj);
            let time = j - i - dist;
            (i, j, dist, time)
        })
    })
}

fn parse(input: &str) -> (Grid<bool>, Position, Position) {
    let mut start = (0, 0);
    let mut end = (0, 0);
    
    let grid = input.lines().enumerate()
        .map(|(r, l)| l.bytes().enumerate().map(|(c, b)| match b {
            b'#' => true,
            b'.' => false,
            b'S' => { start = (r, c); false },
            b'E' => { end = (r, c); false },
            b => unreachable!("{}", char::from(b))
        }).collect())
    .collect();

    (Grid { grid }, start, end)
}
fn path(grid: &Grid<bool>, start: Position, end: Position) -> Vec<Position> {
    let mut spaces = vec![start];
    let mut penult = None;
    let mut last = start;
    while spaces.last() != Some(&end) {
        let next = [(0, 1), (1, 0), (0, -1), (-1, 0)]
            .into_iter()
            .map(|d| translate(last, d))
            .filter(|&p| penult.is_none_or(|q| p != q))
            .find(|&p| grid.get(p).is_some_and(|&w| !w))
            .unwrap();

        spaces.push(next);
        (penult, last) = (Some(last), next);
    }
    spaces
}
pub fn part1(input: &str) -> usize {
    let (grid, start, end) = parse(input);
    let spaces = path(&grid, start, end);

    cheat_iter(&spaces)
        .filter(|&(_, _, d, t)| d == 2 && t >= 100) // limit distance, saves at least 100 ps of time
        .count()
}
pub fn part2(input: &str) -> usize {
    let (grid, start, end) = parse(input);
    let spaces = path(&grid, start, end);

    cheat_iter(&spaces)
        .filter(|&(_, _, d, t)| d <= 20 && t >= 100) // limit distance, saves at least 100 ps of time
        .count()
}

#[cfg(test)]
mod test {
    #[test]
    fn d16_correct() {
        let input = std::fs::read_to_string("inputs/20.txt").unwrap();
        assert_eq!(super::part1(&input), 1384);
        assert_eq!(super::part2(&input), 1008542);
    }
}