// use std::collections::HashSet;

fn main() {
    let input = std::fs::read_to_string("inputs/10.txt").unwrap();
    soln(&input);
}

type Position = (usize, usize);
type PosDelta = (isize, isize);
const DIRECTIONS: [PosDelta; 4] = [
    (0, 1),
    (1, 0),
    (0, -1),
    (-1, 0),
];
fn get_neighbors<T>(grid: &[Vec<T>], (r, c): Position) -> impl Iterator<Item = (usize, usize, &T)> {
    DIRECTIONS.into_iter()
        .map(move |(dr, dc)| (r.wrapping_add_signed(dr), c.wrapping_add_signed(dc)))
        .filter_map(|(r, c)| Some((r, c, grid.get(r)?.get(c)?)))
}

fn soln(input: &str) {
    let data: Vec<Vec<_>> = input.lines().map(|s| s.bytes().map(|b| b - b'0').collect()).collect();

    let zeroes: Vec<_> = data.iter().enumerate()
        .flat_map(|(r, row)| row.iter().enumerate().filter_map(move |(c, &cell)| {
            (cell == 0).then_some((r, c))
        })).collect();
    let mut trailheads = vec![];
    for (zr, zc) in zeroes {
        let mut trailhead = 0;
        // let mut visited = HashSet::new();
        let mut frontier = vec![(zr, zc, 0u8)];
        while let Some((r, c, i)) = frontier.pop() {
            // visited.insert((r, c));
            if i == 9 {
                trailhead += 1;
                continue;
            }

            let x = get_neighbors(&data, (r, c))
                .filter(|(_, _, &nei)| nei == (i + 1))
                // .filter(|&(r, c, _)| !visited.contains(&(r, c)))
                .map(|(r, c, &nei)| (r, c, nei))
                .collect::<Vec<_>>();
            frontier.extend(x);
        }

        trailheads.push(trailhead);
    }
    
    let p1: usize = trailheads.into_iter().sum();
    println!("{p1}");

    // Uncomment visited set for pt1, recomment visited set for pt2
}