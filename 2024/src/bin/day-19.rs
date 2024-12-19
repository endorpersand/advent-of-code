use std::collections::HashMap;
fn main() {
    let input = std::fs::read_to_string("inputs/19.txt").unwrap();
    soln(&input);
}

fn compute<'a>(design: &'a str, patterns: &[&str], cache: &mut HashMap<&'a str, usize>) -> usize {
    if let Some(&n) = cache.get(design) { return n; }
    
    let result = patterns.iter()
        .filter_map(|p| design.strip_prefix(p))
        .map(|sp| compute(sp, patterns, cache))
        .sum();

    cache.insert(design, result);
    result
}
fn soln(input: &str) {
    let mut lines = input.lines();
    let patterns: Vec<_> = lines.next().unwrap().split(", ").collect();
    
    let mut cache = HashMap::from_iter([("", 1)]);
    let combos: Vec<_> = lines.skip(1)
        .map(|d| compute(d, &patterns, &mut cache))
        .filter(|&n| n > 0)
        .collect();
    println!("{}", combos.len());
    println!("{}", combos.iter().sum::<usize>());
}