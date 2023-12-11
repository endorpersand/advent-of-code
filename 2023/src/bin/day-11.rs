use std::collections::BTreeSet;

fn main() {
    let txt = std::fs::read_to_string("inputs/11.txt").unwrap();
    let state = parse(&txt);

    let Expansions { rows, cols } = find_expanded(&state);
    
    let count = state.galaxies.iter()
        .enumerate()
        .flat_map(|(i, &gal1)| {
            state.galaxies[(i + 1)..].iter()
                .map(move |&gal2| (gal1, gal2))
        })

        // calculate path for pair
        .map(|((r1, c1), (r2, c2))| {
            let min_r = r1.min(r2);
            let max_r = r1.max(r2);
            let min_c = c1.min(c2);
            let max_c = c1.max(c2);

            // replace 999999 with 1 to solve pt 1
            let out = (max_c - min_c) + cols.range(min_c..=max_c).count() * 999999 + (max_r - min_r) + rows.range(min_r..=max_r).count() * 999999;
            out
        })
        .sum::<usize>();
    println!("{count}");
}

struct State {
    galaxies: Vec<(usize, usize)>,
    rows: usize,
    cols: usize
}
fn parse(file: &str) -> State {
    let galaxies = file.lines()
        .enumerate()
        .flat_map(|(row, line)| {
            line.bytes().enumerate()
                .filter(|&(_, b)| b == b'#')
                .map(move |(col, _)| (row, col))
        })
        .collect();
    
    let cols = file.find('\n').unwrap();
    let rows = file.lines().count();

    State { galaxies, rows, cols }
}

struct Expansions {
    rows: BTreeSet<usize>,
    cols: BTreeSet<usize>
}
fn find_expanded(state: &State) -> Expansions {
    let mut used_rows = BTreeSet::from_iter(0..state.rows);
    let mut used_cols = BTreeSet::from_iter(0..state.cols);

    for (r, c) in &state.galaxies {
        used_rows.remove(r);
        used_cols.remove(c);
    }

    Expansions {
        rows: used_rows,
        cols: used_cols
    }
}
