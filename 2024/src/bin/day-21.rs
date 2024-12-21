#![recursion_limit = "256"]
use std::collections::HashSet;

fn main() {
    let input = std::fs::read_to_string("inputs/21.txt").unwrap();
    soln(&input);
}

type Position = (usize, usize);

#[derive(Clone, Copy)]
struct Pad<const N: usize> {
    grid: [[u8; 3]; N]
}
const NUMPAD: Pad<4> = Pad {
    grid: [
        *b"789",
        *b"456",
        *b"123",
        *b"\x000A"
    ]
};
const DIRPAD: Pad<2> = Pad {
    grid: [
        *b"\x00^A",
        *b"<v>"
    ]
};

impl<const N: usize> Pad<N> {
    fn find(&self, key: u8) -> Option<Position> {
        self.grid.as_flattened().iter()
            .position(|&byte| byte == key)
            .map(|p| (p / 3, p % 3))
    }

    fn compute(&self, string: &[u8]) -> HashSet<Vec<u8>> {
        let mut astring = b"A".to_vec();
        astring.extend_from_slice(string);

        astring.windows(2)
            .flat_map(<[_; 2]>::try_from)
            .map(|[l, r]| {
                let (lr, lc) = self.find(l).unwrap();
                let (rr, rc) = self.find(r).unwrap();
                let (nr, nc) = self.find(0).unwrap();

                let row_iter = match lr <= rr {
                    true  => std::iter::repeat_n(b'v', rr - lr),
                    false => std::iter::repeat_n(b'^', lr - rr),
                };
                let col_iter = match lc <= rc {
                    true  => std::iter::repeat_n(b'>', rc - lc),
                    false => std::iter::repeat_n(b'<', lc - rc),
                };
                
                match (lc == nc && rr == nr, lr == nr && rc == nc) {
                    (true, false) => vec![col_iter.chain(row_iter).chain(*b"A")],
                    (false, true) => vec![row_iter.chain(col_iter).chain(*b"A")],
                    _ => vec![row_iter.clone().chain(col_iter.clone()).chain(*b"A"), col_iter.clone().chain(row_iter.clone()).chain(*b"A")]
                }
            })
            .fold(HashSet::from_iter([vec![]]), |mut acc, options| {
                let strings = std::mem::take(&mut acc);
                for right in options {
                    acc.extend({
                        strings.clone().into_iter()
                            .map(|mut s| {s.extend(right.clone()); s})
                    });
                }

                acc
            })
    }
}

fn compute3(string: &[u8]) -> Vec<u8> {
    NUMPAD.compute(string).into_iter()
        .flat_map(|p| DIRPAD.compute(&p).into_iter())
        .flat_map(|p| DIRPAD.compute(&p).into_iter())
        .min_by_key(Vec::len)
        .unwrap()
}

fn soln(input: &str) {
    let items: usize = input.lines()
        .map(|l| dbg!(l[..3].parse::<usize>().unwrap(), compute3(l.as_bytes()).len()))
        .map(|(a, b)| a * b)
        .sum();
    // let items: usize = input.lines()
    //     .map(|l| dbg!(l[..3].parse::<usize>().unwrap(), compute25(l.as_bytes()).len()))
    //     .map(|(a, b)| a * b)
    //     .sum();

    println!("{items}");
}