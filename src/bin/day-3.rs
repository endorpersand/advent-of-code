use std::collections::HashSet;
use std::fs;


fn main() {
    let input = fs::read_to_string("inputs/3.txt").unwrap();
    let mut buf = 0;
    
    // PART A
    for racksack in input.lines() {
        let (c1, c2) = racksack.split_at(racksack.len() / 2);
        let set1: HashSet<_> = c1.chars().collect();
        let set2: HashSet<_> = c2.chars().collect();
        let mut intersect = set1.intersection(&set2);

        let mtch = intersect.next().unwrap();
        debug_assert_eq!(intersect.next(), None);
        buf += calculate_priority(*mtch);
    }
    println!("{buf}");

    // PART B
    let mut buf = 0;
    let chunked = Chunk::<3, _>::new(input.lines());

    for chunks in chunked {
        let mtch = chunks.into_iter()
            .map(|s| s.chars().collect::<HashSet<_>>())
            .reduce(|acc, cv| acc.intersection(&cv).copied().collect::<HashSet<_>>())
            .unwrap();
        
        debug_assert_eq!(mtch.len(), 1);
        let c = mtch.into_iter().next().unwrap();
        buf += calculate_priority(c);
    }
    println!("{buf}");
}

// PART A

fn calculate_priority(c: char) -> usize {
    if c.is_ascii_lowercase() {
        c as usize - 0x60
    } else if c.is_ascii_uppercase() {
        c as usize - 0x40 + 26
    } else {
        panic!("Not alphabetic")
    }
}

// PART B
struct Chunk<const N: usize, I> {
    it: I
}
impl<const N: usize, I: Iterator> Chunk<N, I> {
    fn new(it: I) -> Self {
        Self { it }
    }
}
impl<const N: usize, I: Iterator> Iterator for Chunk<N, I> {
    type Item = [I::Item; N];

    fn next(&mut self) -> Option<Self::Item> {
        self.it.by_ref()
            .take(N)
            .collect::<Vec<_>>()
            .try_into()
            .ok()
    }
}