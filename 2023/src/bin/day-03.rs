use std::collections::{HashSet, HashMap};
use std::ops::Range;

use once_cell::sync::Lazy;
use regex::Regex;

fn main() {
    let txt = std::fs::read_to_string("inputs/03.txt").unwrap();

    let adjs = find_adjacents(&txt);
    let ranges = get_ranges(&txt);

    let out: usize = ranges.iter()
        .filter(|(_, range)| adjs.iter().any(|adj| range.contains(adj)))
        .map(|(val, _)| val)
        .sum();
    println!("{out}");

    let adj_mgears = find_adjacent_mgears(&txt);
    let mut ctr: HashMap<_, Vec<_>> = HashMap::new();

    for (val, range) in ranges.iter() {
        let Some(&hit) = adj_mgears.iter()
                .filter(|(adj, _)| range.contains(adj))
                .map(|(_, k)| k)
                .next() else { continue };

        ctr.entry(hit).or_default().push(val);
    }
    let out: usize = ctr.iter()
        .filter(|(_, vals)| vals.len() == 2)
        .map(|(_, vals)| vals[0] * vals[1])
        .sum();
    println!("{out}");
}

fn find_adjacents(file: &str) -> HashSet<usize> {
    let line_len = file.lines().next().unwrap().len();
    let n_lines = file.lines().count();

    let symbols: HashSet<_> = file.lines()
        .enumerate()
        .flat_map(|(lno, line)| {
            line.char_indices()
                .filter(|&(_, c)| c.is_ascii_punctuation() && c != '.')
                .map(move |(cno, _)| (lno, cno))
        })
        .collect();

    symbols.iter()
        .flat_map(|&(lno, cno)| {
            let lnom1 = lno.checked_sub(1);
            let lnop1 = lno.checked_add(1).filter(|&n| n < n_lines);
            let cnom1 = cno.checked_sub(1);
            let cnop1 = cno.checked_add(1).filter(|&n| n < line_len);
            [
                lnom1.zip(cnom1),
                lnom1.zip(Some(cno)),
                lnom1.zip(cnop1),
                Some(lno).zip(cnom1),
                Some(lno).zip(cnop1),
                lnop1.zip(cnom1),
                lnop1.zip(Some(cno)),
                lnop1.zip(cnop1),
            ]
                .into_iter()
                .flatten()
                .map(|(l, c)| l * (line_len + 1) + c)
        })
        .collect()
}

fn get_ranges(file: &str) -> Vec<(usize, Range<usize>)> {
    static RE: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"\d+").unwrap()
    });

    RE.find_iter(file)
        .map(|m| (m.as_str().parse().unwrap(), m.range()))
        .collect()
}

fn find_adjacent_mgears(file: &str) -> Vec<(usize, usize)> {
    let line_len = file.lines().next().unwrap().len();
    let n_lines = file.lines().count();

    let symbols: HashSet<_> = file.lines()
        .enumerate()
        .flat_map(|(lno, line)| {
            line.char_indices()
                .filter(|&(_, c)| c == '*')
                .map(move |(cno, _)| (lno, cno))
        })
        .collect();

    symbols.iter()
        .flat_map(|&(lno, cno)| {
            let lnom1 = lno.checked_sub(1);
            let lnop1 = lno.checked_add(1).filter(|&n| n < n_lines);
            let cnom1 = cno.checked_sub(1);
            let cnop1 = cno.checked_add(1).filter(|&n| n < line_len);
            [
                lnom1.zip(cnom1),
                lnom1.zip(Some(cno)),
                lnom1.zip(cnop1),
                Some(lno).zip(cnom1),
                Some(lno).zip(cnop1),
                lnop1.zip(cnom1),
                lnop1.zip(Some(cno)),
                lnop1.zip(cnop1),
            ]
                .into_iter()
                .flatten()
                .map(|(l, c)| l * (line_len + 1) + c)
                .map(move |p| (p, lno * (line_len + 1) + cno))
        })
        .collect()
}