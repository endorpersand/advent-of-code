use std::collections::HashSet;

fn main() {
    let input = std::fs::read_to_string("inputs/04.txt").unwrap();
    soln2(&input);
}

#[allow(dead_code)]
fn soln1(input: &str) {
    let mut x = HashSet::new();
    let mut m = HashSet::new();
    let mut a = HashSet::new();
    let mut s = HashSet::new();

    for (i, l) in input.lines().enumerate() {
        for (j, c) in l.bytes().enumerate() {
            match c {
                b'X' => x.insert((i, j)),
                b'M' => m.insert((i, j)),
                b'A' => a.insert((i, j)),
                b'S' => s.insert((i, j)),
                _ => unreachable!(),
            };
        }
    }

    const DELTAS: &[[(isize, isize); 3]] = &[
        [(0, 1), (0, 2), (0, 3)],
        [(1, 0), (2, 0), (3, 0)],
        [(1, 1), (2, 2), (3, 3)],
        [(1, -1), (2, -2), (3, -3)],
        [(0, -1), (0, -2), (0, -3)],
        [(-1, 0), (-2, 0), (-3, 0)],
        [(-1, -1), (-2, -2), (-3, -3)],
        [(-1, 1), (-2, 2), (-3, 3)]
    ];
    
    let p1: usize = x.iter()
        .map(|&(i, j)| DELTAS.iter().filter(|&deltas| {
            std::iter::zip([&m, &a, &s], deltas)
                .all(|(set, &(di, dj))| {
                    Option::zip(i.checked_add_signed(di), j.checked_add_signed(dj))
                        .is_some_and(|nc| set.contains(&nc))
                })
        }).count())
        .sum();
    println!("{p1}");

    const DELTAS2: &[[(isize, isize); 4]] = &[
        [(-1, -1), (-1, 1), (1, -1), (1, 1)],
        [(-1, -1), (1, -1), (-1, 1), (1, 1)],
        [(1, -1), (1, 1), (-1, -1), (-1, 1)],
        [(-1, 1), (1, 1), (-1, -1), (1, -1)],
    ];

    let p2: usize = a.iter()
        .map(|&(i, j)| DELTAS2.iter().filter(|&deltas| {
            std::iter::zip([&m, &m, &s, &s], deltas)
                .all(|(set, &(di, dj))| {
                    Option::zip(i.checked_add_signed(di), j.checked_add_signed(dj))
                        .is_some_and(|nc| set.contains(&nc))
                })
        }).count())
        .sum();
    println!("{p2}");
}

#[allow(dead_code)]
fn soln2(input: &str) {
    let [mut x, mut m, mut a, mut s] = std::array::from_fn(|_| HashSet::new());

    for (i, l) in input.lines().enumerate() {
        for (j, c) in l.bytes().enumerate() {
            match c {
                b'X' => x.insert((i, j)),
                b'M' => m.insert((i, j)),
                b'A' => a.insert((i, j)),
                b'S' => s.insert((i, j)),
                _ => unreachable!(),
            };
        }
    }

    fn count_deltas<const N: usize>(
        (i, j): (usize, usize), 
        sets: [&HashSet<(usize, usize)>; N], 
        deltas: &[[(isize, isize); N]]
    ) -> usize {
        deltas.iter()
            .filter(|&&group| {
                std::iter::zip(sets, group).all(|(set, (di, dj))| {
                    i.checked_add_signed(di).zip(j.checked_add_signed(dj)) // coord in bounds
                        .is_some_and(|nc| set.contains(&nc)) // is in set
                })
            })
            .count()
    }

    const DELTAS: &[[(isize, isize); 3]] = &[
        [(0, 1), (0, 2), (0, 3)],
        [(1, 0), (2, 0), (3, 0)],
        [(1, 1), (2, 2), (3, 3)],
        [(1, -1), (2, -2), (3, -3)],
        [(0, -1), (0, -2), (0, -3)],
        [(-1, 0), (-2, 0), (-3, 0)],
        [(-1, -1), (-2, -2), (-3, -3)],
        [(-1, 1), (-2, 2), (-3, 3)]
    ];
    
    let p1: usize = x.iter()
        .map(|&c| count_deltas(c, [&m, &a, &s], DELTAS))
        .sum();
    println!("{p1}");

    const DELTAS2: &[[(isize, isize); 4]] = &[
        [(-1, -1), (-1, 1), (1, -1), (1, 1)],
        [(-1, -1), (1, -1), (-1, 1), (1, 1)],
        [(1, -1), (1, 1), (-1, -1), (-1, 1)],
        [(-1, 1), (1, 1), (-1, -1), (1, -1)],
    ];

    let p2: usize = a.iter()
        .map(|&c| count_deltas(c, [&m, &m, &s, &s], DELTAS2))
        .sum();
    println!("{p2}");
}