use std::cmp::Ordering;
use std::collections::HashMap;

fn main() {
    let txt = std::fs::read_to_string("inputs/12.txt").unwrap();
    let State { springs } = parse(&txt);

    let mut cache: Cache = Cache::new();

    bench(|| {
        let out: usize = springs.iter()
            .cloned()
            .map(SpringConfigB::new)
            .map(|spring| spring.count_possibilities_b(&mut cache))
            .sum();
        assert_eq!(out, 7379);
        println!("{out}");
    });

    bench(|| {
        let out: usize = springs.iter()
            .cloned()
            .map(SpringConfig::convert_to_config_b)
            .map(SpringConfigB::new)
            .map(|spring| spring.count_possibilities_b(&mut cache))
            .sum();
        assert_eq!(out, 7732028747925);
        println!("{out}");
    });
}

fn bench(f: impl FnOnce()) {
    use std::time::Instant;

    println!("=== beginning computation ===");
    let start = Instant::now();
    f();
    let end = Instant::now();

    println!("=== complete. elapsed time: {:?} ===", end - start);
}
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
#[repr(u8)]
enum Cond {
    Operational = b'.',
    Damaged = b'#',
    Unknown = b'?',
}
#[derive(Clone, PartialEq, Eq, Debug)]
struct SpringConfig {
    text: Vec<Cond>,
    count: Vec<usize>
}
#[derive(Debug)]
struct State {
    springs: Vec<SpringConfig>,
}
fn parse(file: &str) -> State {
    let springs = file.lines()
        .map(|line| {
            let (text_str, count_str) = line.split_once(' ').unwrap();

            let text = text_str.bytes()
                .map(|b| match b {
                    b'.' => Cond::Operational,
                    b'#' => Cond::Damaged,
                    b'?' => Cond::Unknown,
                    _ => unreachable!()
                })
                .collect();

            let count = count_str.split(',')
                .map(|s| s.parse().unwrap())
                .collect();

            SpringConfig { text, count }
        })
        .collect();

    State { springs }
}

impl SpringConfig {
    fn convert_to_config_b(self) -> SpringConfig {
        let Self { mut text, count } = self;
        text.push(Cond::Unknown);
        let mut text = text.repeat(5);
        text.pop();

        let count = count.repeat(5);

        Self { text, count }
    }
}

type Cache = HashMap<SpringConfigB, usize>;

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
struct DamageBlock {
    // The damage block exists in the least significant `size` bits of this buffer.
    // The line configuration starts from the LSB and moves to the MSB.
    buffer: u128,
    size: u8
}
impl DamageBlock {
    fn new(buffer: u128, size: u8) -> Self {
        // buffer: clear out unused bits to appease equality/hash
        Self { buffer: buffer & ((1 << size) - 1), size }
    }
    fn size(&self) -> usize {
        self.size as usize
    }
    fn fits(&self, ct: usize) -> bool {
        ct <= self.size()
    }
    fn fits_n(&self, cts: &[usize]) -> bool {
        cts.is_empty() || cts.iter().sum::<usize>() + cts.len() - 1 <= self.size()
    }
    fn count_knowns(&self) -> usize {
        self.buffer.count_ones() as usize
    }
    fn take(&self, n: u8) -> Option<DamageBlock> {
        match self.size.cmp(&n) {
            Ordering::Less    => None,
            Ordering::Equal   => Some(Self::new(0, 0)),
            Ordering::Greater => {
                // nth item in buffer has to be 0 
                // (since we're emulating taking an operational)
                let buf = self.buffer >> n;
                (buf & 1 == 0).then(|| Self::new(buf >> 1, self.size - n - 1))
            }
        }
    }
    fn count_possibilities(&self, cts: &[usize], cache: &mut Cache) -> usize {
        let cfg = SpringConfigB { blocks: vec![self.clone()], count: cts.to_vec() };
        if let Some(&pos) = cache.get(&cfg) { return pos; }
        
        let Some((&ct0, ct_rest)) = cts.split_first() else {
            // if counts is empty,
            // there is one possibility if there are no knowns
            // and no possibilities if there are knowns
                return usize::from(self.count_knowns() == 0);
        };
        // number of trailing ?s
        let bl = (self.buffer.trailing_zeros() as u8).min(self.size);

        let pos = (0..=bl).map(|shift| DamageBlock::new(self.buffer >> shift, self.size - shift))
            .filter_map(|block| block.take(ct0 as u8))
            .map(|block| block.count_possibilities(ct_rest, cache))
            .sum::<usize>();

        cache.insert(cfg, pos);

        pos
    }
}

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
struct SpringConfigB {
    blocks: Vec<DamageBlock>,
    count: Vec<usize>
}

impl SpringConfigB {
    fn new(cfg: SpringConfig) -> Self {
        let SpringConfig { text, count } = cfg;
        
        let blocks = text.split(|&k| k == Cond::Operational)
            .filter(|s| !s.is_empty())
            .map(|s| {
                let size = s.len();
                let buffer = s.iter()
                    .rfold(0, |acc, &cv| acc << 1 | u128::from(cv == Cond::Damaged));

                DamageBlock::new(buffer, u8::try_from(size).unwrap())
            })
            .collect();

        SpringConfigB { blocks, count }
    }

    fn count_possibilities_b(&self, cache: &mut Cache) -> usize {
        if self.count.is_empty() {
            return usize::from(self.blocks.iter().all(|b| b.count_knowns() == 0));
        }
        if self.blocks.is_empty() {
            // by check above, there are counts left,
            // but no more blocks are left
            return 0;
        }

        if let Some(&pos) = cache.get(self) { return pos; }

        // for every n in 0..len(count), 
        // include count[0..n] in first block, and count[n..] in remaining
        let (b1, brest) = self.blocks.split_first()
            .expect("block had at least 1 el");
        
        let mut possibilities: usize = 0;

        for i in 0..=self.count.len() {
            let first_cts = &self.count[0..i];
            if !b1.fits_n(first_cts) { break; }

            let first_pos = b1.count_possibilities(first_cts, cache);

            let rest_cfg = SpringConfigB {
                blocks: brest.to_vec(),
                count: self.count[i..].to_vec()
            };
            let rest_pos = rest_cfg.count_possibilities_b(cache);

            possibilities += first_pos * rest_pos;
        }

        cache.insert(self.clone(), possibilities);
        possibilities
    }
}