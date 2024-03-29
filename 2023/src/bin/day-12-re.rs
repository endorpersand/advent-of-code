use std::borrow::Cow;
use std::cmp::Ordering;
use std::num::NonZeroUsize;

use rayon::prelude::*;
use rustc_hash::FxHashMap;

fn main() {
    let txt = std::fs::read_to_string("inputs/12.txt").unwrap();

    bench(|| {
        let rec: Vec<_> = txt.lines()
            .map(Line::parse)
            .map(SpringRecord::new)
            .collect();

        let out: usize = rec.par_iter()
            .map_init(
                Cache::default,
                |cache, spring| count_possible(&spring.blocks, &spring.count, cache)
            )
            .sum();
        assert_eq!(out, 7379);
        println!("{out}");
    });

    bench(|| {
        let rec: Vec<_> = txt.lines()
            .map(Line::parse)
            .map(Line::convert_to_config_b)
            .map(SpringRecord::new)
            .collect();

        let out: usize = rec.par_iter()
            .map_init(
                Cache::default,
                |cache, spring| count_possible(&spring.blocks, &spring.count, cache)
            )
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

///// PARSING 
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
#[repr(u8)]
enum Cond {
    Operational = b'.',
    Damaged = b'#',
    Unknown = b'?',
}

#[derive(Clone, PartialEq, Eq, Debug)]
struct Line<'s> {
    text: Cow<'s, [Cond]>,
    count: Vec<u8>
}
impl Line<'_> {
    fn parse(line: &str) -> Self {
        let (text_str, count_str) = line.split_once(' ').unwrap();
    
        let text = Cow::Borrowed(unsafe {
            std::mem::transmute::<&[u8], &[Cond]>(text_str.as_bytes())
        });
    
        let count = count_str.split(',')
            .map(|s| s.parse().unwrap())
            .collect();
    
        Self { text, count }
    }

    fn convert_to_config_b(self) -> Line<'static> {
        let Self { text, count } = self;
        
        let mut text = text.into_owned();
        text.push(Cond::Unknown);
        text.repeat(5);
        let mut text = text.repeat(5);
        text.pop();
    
        let count = count.repeat(5);
    
        Line { text: Cow::from(text), count }
    }
}
/////

///// CACHE UTILITIES
type Cache<'k> = FxHashMap<SpringRecordKey<'k>, usize>;
#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
struct SpringRecordKey<'k> {
    blocks: BlockKey<'k>,
    count: &'k [u8]
}
#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
enum BlockKey<'k> {
    One(DamageBlock),
    Many(&'k [DamageBlock])
}
/////

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
struct DamageBlock {
    // The damage block exists in the least significant `size` bits of this buffer.
    // The line configuration starts from the LSB and moves to the MSB.
    //
    // The most significant 8 bits are the size.
    buffer: u128
}
impl DamageBlock {
    fn new(buffer: u128, size: u8) -> Self {
        // buffer: clear out unused bits to appease equality/hash
        Self { buffer: buffer & ((1 << size) - 1) | (u128::from(size) << 120) }
    }
    fn size(&self) -> u8 {
        (self.buffer >> 120) as u8
    }
    fn fits(&self, cts: &[u8]) -> bool {
        let s = cts.iter().copied().fold(0u8, u8::saturating_add);

        // if cts has less knowns than this block has, it cannot pass
        // if cts requires more space than exists in this block, then it also cannot pass
        self.count_knowns() <= s && s.saturating_sub(1).saturating_add(cts.len() as u8) <= self.size()
    }
    fn count_knowns(&self) -> u8 {
        (self.buffer & ((1 << 120) - 1)).count_ones() as u8
    }
    fn take(&self, n: u8) -> Option<DamageBlock> {
        match self.size().cmp(&n) {
            Ordering::Less    => None,
            Ordering::Equal   => Some(Self::new(0, 0)),
            Ordering::Greater => {
                // nth item in buffer has to be 0 
                // (since we're emulating taking an operational)
                let buf = self.buffer >> n;
                (buf & 1 == 0).then(|| Self::new(buf >> 1, self.size() - n - 1))
            }
        }
    }
    fn count_possible<'k>(&self, cts: &'k [u8], cache: &mut Cache<'k>) -> usize {
        if !self.fits(cts) { return 0 };

        let Some((&ct0, ct_rest)) = cts.split_first() else {
            // if counts is empty,
            // there is one possibility if there are no knowns
            // and no possibilities if there are knowns
            return usize::from(self.count_knowns() == 0);
        };

        let cfg = SpringRecordKey { blocks: BlockKey::One(*self), count: cts };
        if let Some(&pos) = cache.get(&cfg) { return pos; }

        // number of trailing ?s
        let bl = (self.buffer.trailing_zeros() as u8).min(self.size());

        let pos = (0..=bl)
            .filter_map(|shift| self.take(shift + ct0))
            .map(|block| block.count_possible(ct_rest, cache))
            .sum::<usize>();

        cache.insert(cfg, pos);

        pos
    }
}

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
struct SpringRecord {
    blocks: Vec<DamageBlock>,
    count: Vec<u8>
}

impl SpringRecord {
    fn new(cfg: Line<'_>) -> Self {
        let Line { text, count } = cfg;
        
        let blocks = text.split(|&k| k == Cond::Operational)
            .filter(|s| !s.is_empty())
            .map(|s| {
                let size = s.len();
                let buffer = s.iter()
                    .rfold(0, |acc, &cv| acc << 1 | u128::from(cv == Cond::Damaged));

                DamageBlock::new(buffer, u8::try_from(size).unwrap())
            })
            .collect();

        SpringRecord { blocks, count }
    }
}

fn count_possible<'k>(blocks: &'k [DamageBlock], count: &'k [u8], cache: &mut Cache<'k>) -> usize {
    if count.is_empty() {
        return usize::from(blocks.iter().all(|b| b.count_knowns() == 0));
    }

    // for every n in 0..len(count), 
    // include count[0..n] in first block, and count[n..] in remaining
    let Some((b0, b_rest)) = blocks.split_first() else { return 0 };

    let cfg = SpringRecordKey { blocks: BlockKey::Many(blocks), count };
    if let Some(&pos) = cache.get(&cfg) { return pos; }

    let mut possibilities: usize = 0;

    for i in 0..=count.len() {
        let (ct_left, ct_right) = count.split_at(i);

        // short circuit optimization
        let Some(left_pos) = NonZeroUsize::new(b0.count_possible(ct_left, cache)) else { continue };
        let right_pos = count_possible(b_rest, ct_right, cache);
        possibilities += left_pos.get() * right_pos;
    }

    cache.insert(cfg, possibilities);
    possibilities
}