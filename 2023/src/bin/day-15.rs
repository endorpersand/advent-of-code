use indexmap::IndexMap;

fn main() {
    let txt = std::fs::read_to_string("inputs/15.txt").unwrap();
    let out: usize = txt.split(',')
        .map(|s| hash(s) as usize)
        .sum();
    println!("{out}");

    let mut cells: [_; 256] = std::array::from_fn(|_| IndexMap::new());
    for instr in txt.split(',') {
        if let Some((label, len)) = instr.split_once('=') {
            cells[hash(label) as usize].insert(label, len.parse::<u8>().unwrap());
        } else {
            let label = instr.strip_suffix('-').unwrap();
            cells[hash(label) as usize].shift_remove(&label);
        }
    }
    let out: usize = cells.into_iter()
        .zip(1usize..)
        .flat_map(|(cell, i)| {
            cell.into_values()
                .zip(1usize..)
                .map(move |(len, j)| i * j * (len as usize))
        })
        .sum();
    println!("{out}");
}


fn hash(s: &str) -> u8 {
    s.bytes()
        .fold(0, |acc, cv| acc.wrapping_add(cv).wrapping_mul(17))
}