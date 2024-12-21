use std::collections::HashMap;

fn main() {
    let input = std::fs::read_to_string("inputs/21.txt").unwrap();
    soln(&input);
}

type Position = (usize, usize);
type EdgeMap = HashMap<[u8; 2], Vec<Vec<u8>>>;
type Cache = HashMap<Vec<u8>, HashMap<usize, usize>>;

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

    fn path(&self, l: u8, r: u8) -> Vec<Vec<u8>> {
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
        
        let crit = || col_iter.clone().chain(row_iter.clone()).chain(*b"A").collect();
        let rcit = || row_iter.clone().chain(col_iter.clone()).chain(*b"A").collect();
        match (lc == nc && rr == nr, lr == nr && rc == nc) {
            (true, false) => vec![crit()],
            (false, true) => vec![rcit()],
            _ if lr == rr || lc == rc => vec![crit()],
            _ => vec![rcit(), crit()]
        }
    }

    fn compute(&self, string: &[u8]) -> Vec<Vec<u8>> {
        [b"A", string]
            .concat()
            .windows(2)
            .flat_map(<[_; 2]>::try_from)
            .map(|[l, r]| self.path(l, r))
            .fold(vec![vec![]], |acc, options| {
                acc.into_iter().flat_map(|left| {
                    options.iter().map(move |right| {
                        [left.as_slice(), right.as_slice()].concat()
                    })
                })
                .collect()
            })
    }
}

fn count_dir(string: &[u8], n: usize, edge_map: &EdgeMap, cache: &mut Cache) -> usize {
    if n == 0 { return string.len(); }
    if let Some(map) = cache.get(string) {
        if let Some(&count) = map.get(&n) {
            return count;
        }
    }

    let result = [b"A", string]
        .concat()
        .windows(2)
        .flat_map(<[_; 2]>::try_from)
        .map(|key| {
            edge_map[&key].iter()
                .map(|s| count_dir(s, n - 1, edge_map, cache))
                .min()
                .unwrap()
        })
        .sum();

    *cache.entry(string.to_vec())
        .or_default()
        .entry(n)
        .insert_entry(result)
        .get()
}

fn count(string: &[u8], n: usize, path2_map: &EdgeMap, cache: &mut Cache) -> usize {
    NUMPAD.compute(string).into_iter()
        .map(|s| count_dir(&s, n, path2_map, cache))
        .min()
        .unwrap()
}

fn soln(input: &str) {
    let seqs: Vec<_> = input.lines()
        .map(|s| (s[..3].parse().unwrap(), s.as_bytes()))
        .collect();
    let path2_map: EdgeMap = {
        let flat = DIRPAD.grid.as_flattened();

        HashMap::from_iter({
            flat.iter()
                .flat_map(|&l| flat.iter().map(move |&r| ([l, r], DIRPAD.path(l, r))))
        })
    };
    let mut cache: Cache = HashMap::new();

    let p1: usize = seqs.iter()
        .map(|(n, s)| n * count(s, 2, &path2_map, &mut cache))
        .sum();
    println!("{p1}");
    
    let p2: usize = seqs.iter()
        .map(|(n, s)| n * count(s, 25, &path2_map, &mut cache))
        .sum();
    println!("{p2}");
}