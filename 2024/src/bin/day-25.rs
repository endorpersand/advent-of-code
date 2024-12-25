fn main() {
    let input = std::fs::read_to_string("inputs/25.txt").unwrap();
    soln(&input);
}

#[derive(Clone, Copy, Debug)]
struct Grid {
    grid: [[u8; 5]; 7]
}
impl Grid {
    fn to_lockpin(self) -> Lockpin {
        let is_lock = &self.grid[0] == b"#####";
        let heights = self.grid.into_iter()
            .fold([0; 5], |acc, cv| std::array::from_fn(|i| acc[i] + u8::from(cv[i] == b'#')))
            .map(|i| i - 1);

        Lockpin { is_lock, heights }
    }
}
#[derive(Clone, Copy, Debug)]
struct Lockpin {
    is_lock: bool,
    heights: [u8; 5]
}
impl Lockpin {
    fn fits(self, other: Lockpin) -> bool {
        std::iter::zip(self.heights, other.heights)
            .all(|(a, b)| a + b <= 5)
    }
}
fn parse(input: &str) -> Vec<Grid> {
    let mut grids = vec![];
    let mut lines = input.lines();

    loop {
        let grid = lines.by_ref().take(7)
            .flat_map(|l| <&[_; 5]>::try_from(l.as_bytes()))
            .copied()
            .collect::<Box<_>>();
        grids.push(Grid { grid: *Box::try_from(grid).unwrap() });
        if lines.next().is_none() { break; }
    }

    grids
}
fn soln(input: &str) {
    let grids = parse(input);
    let (locks, pins): (Vec<_>, _) = grids.into_iter()
        .map(Grid::to_lockpin)
        .partition(|lp| lp.is_lock);
    let count = locks.iter().flat_map(|&l| {
        pins.iter().filter(move |&&p| l.fits(p))
    })
    .count();
    println!("{count:?}");
}
