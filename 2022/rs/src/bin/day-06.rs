use std::collections::HashSet;
use std::fs;

fn main() {
    let input = fs::read_to_string("inputs/6.txt").unwrap();
    println!("{}", find_first_non_match(&input, 4));
    println!("{}", find_first_non_match(&input, 14))
}

fn find_first_non_match(s: &str, chunk: usize) -> usize {
    let len = s.len();
    for i in chunk..len {
        if s[(i - chunk)..i].chars().collect::<HashSet<_>>().len() == chunk {
            return i;
        }
    }

    panic!(":(")
}