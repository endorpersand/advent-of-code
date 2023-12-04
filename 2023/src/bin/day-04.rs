use std::collections::{HashSet, HashMap};

use once_cell::sync::Lazy;
use regex::Regex;

fn main() {
    let txt = std::fs::read_to_string("inputs/04.txt").unwrap();
    
    let n_wins: HashMap<_, _> = txt.lines()
        .map(parse_card_basic)
        .collect();

    let out: usize = n_wins.values()
        .map(|&hits| if hits != 0 { 1 << (hits - 1) } else { 0 })
        .sum();
    println!("{out}");

    let n_lines = txt.lines().count();
    let mut ctr = vec![1usize; n_lines];
    for i in 0..n_lines {
        let n_cards = ctr[i];
        let n_matches = n_wins[&(i + 1)];

        #[allow(clippy::needless_range_loop)]
        for j in (i + 1)..(i + 1 + n_matches) {
            ctr[j] += n_cards;
        }
    }
    println!("{}", ctr.iter().sum::<usize>());
}

fn parse_card_basic(line: &str) -> (usize, usize) {
    static RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"\d+").unwrap());

    let (number, card) = line.split_once(": ").unwrap();
    let cno = RE.find(number).unwrap()
        .as_str()
        .parse()
        .unwrap();

    let (winning, owned) = card.split_once(" | ").unwrap();

    let winning_numbers: HashSet<usize> = RE.find_iter(winning)
        .map(|t| t.as_str().parse().unwrap())
        .collect();
    let owned_numbers: HashSet<usize> = RE.find_iter(owned)
        .map(|t| t.as_str().parse().unwrap())
        .collect();

    let matches = winning_numbers.intersection(&owned_numbers)
        .count();

    (cno, matches)
}