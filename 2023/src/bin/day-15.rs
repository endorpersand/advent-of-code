use indexmap::IndexMap;

fn main() {
    // PART A
    let txt = std::fs::read_to_string("inputs/15.txt").unwrap();
    let out: usize = txt.split(',').map(hash).sum();
    println!("{out}");

    // PART B
    let mut cells: [_; 256] = std::array::from_fn(|_| IndexMap::new());
    for instr in txt.split(',') {
        let (label, len) = instr.split_once(|c: char| c.is_ascii_punctuation()).unwrap();
        match len.parse::<usize>() {
            Ok(t)  => cells[hash(label)].insert(label, t),
            Err(_) => cells[hash(label)].shift_remove(&label),
        };
    }
    let out: usize = cells.into_iter()
        .enumerate()
        .flat_map(|(i, cell)| {
            cell.into_values()
                .enumerate()
                .map(move |(j, len)| (i + 1) * (j + 1) * len)
        })
        .sum();
    println!("{out}");
}


fn hash(s: &str) -> usize {
    s.bytes().fold(0u8, |acc, cv| acc.wrapping_add(cv).wrapping_mul(17)) as usize
}