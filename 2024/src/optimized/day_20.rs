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

fn ring(n: usize) -> impl Iterator<Item=PosDelta> {
    let n = n as isize;
    (0..n).flat_map(move |i| [(i, n - i), (n - i, -i), (-i, -n + i), (-n + i, i)])
}
/// Takes a list of positions (sorted by distance from start) 
/// and produces an iterator of time saved.
fn cheat_iter<'a>(spaces: &'a [Position], rev_spaces: &'a Grid<usize>, n: usize) -> impl Iterator<Item=usize> + 'a {
    spaces.iter().enumerate()
        .flat_map(move |(i, &pi)| {
            ring(n).map(move |d| translate(pi, d))
                .filter_map(|np| rev_spaces.get(np).filter(|&&v| v != usize::MAX).copied())
                .filter(move |&j| i <= j)
                .map(move |j| j - i - n)
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
fn path(grid: &Grid<bool>, start: Position, end: Position) -> (Vec<Position>, Grid<usize>) {
    let mut spaces = vec![start];
    
    let mut rev_spaces = Grid { grid: vec![vec![usize::MAX; grid.grid[0].len()]; grid.grid.len()] };
    rev_spaces.grid[start.0][start.1] = 0;

    let mut penult = None;
    let mut last = start;
    while last != end {
        let next = [(0, 1), (1, 0), (0, -1), (-1, 0)]
            .into_iter()
            .map(|d| translate(last, d))
            .filter(|&p| penult.is_none_or(|q| p != q))
            .find(|&p| grid.get(p).is_some_and(|&w| !w))
            .unwrap();

        spaces.push(next);
        rev_spaces.grid[next.0][next.1] = spaces.len() - 1;
        (penult, last) = (Some(last), next);
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

    (2..=20).map(|n| {
        cheat_iter(&spaces, &rev_spaces, n)
            .filter(|&t| t >= 100) // limit distance, saves at least 100 ps of time
            .count()
    }).sum()
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