use std::cmp::Ordering;
use std::collections::HashMap;
use std::num::NonZeroUsize;

fn main() {
    let txt = std::fs::read_to_string("inputs/12.txt").unwrap();
    let State { springs } = parse(&txt);

    let mut cache = Cache::new();

    bench(|| {
        let out: usize = springs.iter()
            .cloned()
            .map(SpringConfigB::new)
            .map(|spring| count_possible(&spring.blocks, &spring.count, &mut cache))
            .sum();
        assert_eq!(out, 7379);
        println!("{out}");
    });

    bench(|| {
        let out: usize = springs.iter()
            .cloned()
            .map(SpringConfig::convert_to_config_b)
            .map(SpringConfigB::new)
            .map(|spring| count_possible(&spring.blocks, &spring.count, &mut cache))
            .sum();
        assert_eq!(out, 7732028747925);
        println!("{out}");
    });

    println!("{}", cache.len());
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
    fn fits(&self, cts: &[usize]) -> bool {
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
    fn count_possible(&self, cts: &[usize], cache: &mut Cache) -> usize {
        let Some((&ct0, ct_rest)) = cts.split_first() else {
            // if counts is empty,
            // there is one possibility if there are no knowns
            // and no possibilities if there are knowns
            return usize::from(self.count_knowns() == 0);
        };

        let cfg = SpringConfigB { blocks: vec![self.clone()], count: cts.to_vec() };
        if let Some(&pos) = cache.get(&cfg) { return pos; }

        // number of trailing ?s
        let bl = (self.buffer.trailing_zeros() as u8).min(self.size);

        let pos = (0..=bl)
            .map(|shift| DamageBlock::new(self.buffer >> shift, self.size - shift))
            .filter_map(|block| block.take(ct0 as u8))
            .map(|block| block.count_possible(ct_rest, cache))
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
}

fn count_possible(blocks: &[DamageBlock], count: &[usize], cache: &mut Cache) -> usize {
    if count.is_empty() {
        return usize::from(blocks.iter().all(|b| b.count_knowns() == 0));
    }
    if blocks.is_empty() {
        // by check above, there are counts left,
        // but no more blocks are left
        return 0;
    }

    let cfg = SpringConfigB { blocks: blocks.to_vec(), count: count.to_vec() };
    if let Some(&pos) = cache.get(&cfg) { return pos; }

    // for every n in 0..len(count), 
    // include count[0..n] in first block, and count[n..] in remaining
    let (b0, b_rest) = blocks.split_first()
        .expect("block had at least 1 el");
    
    let mut possibilities: usize = 0;

    for i in 0..=count.len() {
        let (ct_left, ct_right) = count.split_at(i);
        if !b0.fits(ct_left) { break; }

        // short circuit optimization
        let choice_pos = NonZeroUsize::new(b0.count_possible(ct_left, cache))
            .map_or(0, |l| l.get() * count_possible(b_rest, ct_right, cache));
        possibilities += choice_pos;
    }

    cache.insert(cfg, possibilities);
    possibilities
}