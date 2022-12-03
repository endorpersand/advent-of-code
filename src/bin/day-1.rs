use std::fs;

fn main() {
    let input = fs::read_to_string("inputs/1.txt").unwrap();
    
    // PART A
    let mut calories = vec![];
    let mut buf: usize = 0;

    for line in input.lines() {
        if line.is_empty() {
            calories.push(buf);
            buf = 0;
        } else {
            buf += line.parse::<usize>().unwrap();
        }
    }
    calories.push(buf);

    println!("{:?}", calories.iter().max());
    
    // PART B
    calories.sort_unstable();
    
    let result: usize = calories.iter().rev()
        .take(3)
        .sum();
    println!("{result}");
}