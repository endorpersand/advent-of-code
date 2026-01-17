use std::cmp::Ordering;
use std::collections::HashMap;

use itertools::Itertools;

fn main() {
    let txt = std::fs::read_to_string("inputs/12.txt").unwrap();
    let State { springs } = parse(&txt);
    
    let out: usize = springs.iter()
        .map(|spring| spring.count_possibilities_a())
        .sum();
    println!("{out}");

    let springs: Vec<_> = springs.into_iter()
        .map(SpringConfig::convert_to_config_b)
        .map(SpringConfigB::new)
        .collect();
    
    let mut cache: Cache = Cache::new();
    let out: usize = springs.iter()
        .map(|spring| spring.count_possibilities_b(&mut cache))
        .sum();
    println!("{out}");
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Fill {
    Operational,
    Damaged
}
#[derive(Clone, PartialEq, Eq, Debug)]
struct SpringConfig {
    text: Vec<Option<Fill>>,
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
                    b'.' => Some(Fill::Operational),
                    b'#' => Some(Fill::Damaged),
                    b'?' => None,
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
    fn count_possibilities_a(&self) -> usize {
        let total_broken = self.count.iter().sum::<usize>();
        
        let known_broken = self.text.iter().filter(|t| matches!(t, Some(Fill::Damaged))).count();
        let unknowns = self.text.iter().filter(|t| t.is_none()).count();

        let Some(to_be_broken) = total_broken.checked_sub(known_broken) else { return 0 };

        (0..unknowns).combinations(to_be_broken)
            .map(|c| {
                // construct combination:
                let mut v = vec![false; unknowns];
                for i in c { v[i] = true; }
                v
            })
            .map(|c| {
                let mut cit = c.into_iter();
                let pack: Vec<_> = self.text.iter()
                    .copied()
                    .map(|mf| mf.unwrap_or_else(|| if cit.next().unwrap() { Fill::Damaged } else { Fill::Operational }))
                    .collect();
                assert_eq!(cit.next(), None);

                pack.into_iter()
                    .group_by(|&k| k)
                    .into_iter()
                    .filter(|(f, _)| f == &Fill::Damaged)
                    .map(|(_, gr)| gr.count())
                    .collect::<Vec<_>>()
            })
            .filter(|v| v == &self.count)
            .count()
    }

    fn convert_to_config_b(self) -> SpringConfig {
        let Self { mut text, count } = self;
        text.push(None);
        let mut text = text.repeat(5);
        text.pop();

        let count = count.repeat(5);

        Self { text, count }
    }
}

// PART B

type Cache = HashMap<SpringConfigB, usize>;

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
struct DamageBlock {
    known_start: bool,
    block: Vec<usize>
}
impl DamageBlock {
    fn new(mut known_start: bool, mut block: Vec<usize>) -> Self {
        let nz = block.iter().position(|&p| p != 0).unwrap_or(block.len());
        let nz_block = block.split_off(nz);

        known_start ^= !block.len().is_multiple_of(2);
        Self { known_start, block: nz_block }
    }

    fn fits(&self, ct: usize) -> bool {
        ct <= self.block.iter().sum::<usize>()
    }
    fn fits_n(&self, cts: &[usize]) -> bool {
        if cts.is_empty() { true } else { self.fits(cts.iter().sum::<usize>() + cts.len() - 1) }
    }
    fn take(&self, n: usize) -> Option<DamageBlock> {
        assert_ne!(n, 0);

        let (&b, rest) = self.block.split_first()?;
        match b.cmp(&n) {
            Ordering::Less => {
                DamageBlock::new(!self.known_start, rest.to_vec())
                    .take(n - b)
            },
            Ordering::Equal => {
                if rest.is_empty() {
                    Some(DamageBlock::new(!self.known_start, vec![]))
                } else {
                    match self.known_start {
                        true => {
                            let mut block = rest.to_vec();
                            block[0] -= 1;
                            Some(DamageBlock::new(!self.known_start, block))
                        },
                        false => None,
                    }
                }
            },
            Ordering::Greater => {
                match self.known_start {
                    true => None,
                    false => {
                        let mut block = self.block.clone();
                        block[0] -= n + 1;
                        Some(DamageBlock::new(self.known_start, block))
                    }
                }
            },
        }
    }

    fn count_possibilities(&self, cts: &[usize], cache: &mut Cache) -> usize {
        let cfg = SpringConfigB {
            blocks: vec![self.clone()],
            count: cts.to_vec()
        };
        if let Some(&pos) = cache.get(&cfg) { return pos };
        
        let pos = if cts.is_empty() {
            if self.count_knowns() == 0 { 1 } else { 0 }
        } else if self.block.is_empty() {
            0
        } else {
            let Some(&bl) = self.block.first() else { panic!("block shouldn't be empty") };
            match self.known_start {
                true => self.take(cts[0]).map_or(0, |t| t.count_possibilities(&cts[1..], cache)),
                false => {
                    (0..=bl).filter_map(|i| {
                        let mut new_block = self.block.clone();
                        new_block[0] = i;

                        DamageBlock::new(self.known_start, new_block)
                            .take(cts[0])
                    })
                    .map(|block| block.count_possibilities(&cts[1..], cache))
                    .sum::<usize>()
                },
            }
        };
        
        cache.insert(cfg, pos);
        pos
    }
    fn count_knowns(&self) -> usize {
        let mut blit = self.block.iter();
        if !self.known_start { blit.next(); }

        blit.step_by(2).sum()
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
        
        let blocks = text.into_iter()
            .group_by(|&k| k)
            .into_iter()
            .map(|(k, gr)| (k, gr.count()))
            .group_by(|&(k, _)| matches!(k, Some(Fill::Operational)))
            .into_iter()
            .filter(|&(operational, _)| !operational)
            .map(|(_, mut gr)| {
                let (first_fill, first_block) = gr.next().unwrap();
                
                let known_start = first_fill.is_some();
                let mut block = vec![first_block];
                block.extend(gr.map(|(_, ct)| ct));

                DamageBlock::new(known_start, block)
            })
            .collect();

        SpringConfigB { blocks, count }
    }

    fn count_possibilities_b(&self, cache: &mut Cache) -> usize {
        if self.count.is_empty() {
            return match self.blocks.iter().all(|b| b.count_knowns() == 0) {
                true  => 1,
                false => 0,
            }
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