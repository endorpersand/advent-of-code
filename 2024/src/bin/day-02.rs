fn main() {
    let input = std::fs::read_to_string("inputs/02.txt").unwrap();

    let data: Vec<_> = input.lines()
        .map(|s| {
            s.split_whitespace()
                .map(|s| s.parse::<usize>())
                .collect::<Result<Vec<_>, _>>()
                .unwrap()
        })
        .collect();

    let x: usize = data.iter()
        .filter(|line| is_safe(line))
        .count();
    println!("{x}");

    let y: usize = data.iter()
        .filter(|&line| {
            (0..line.len()).any(|i| {
                let mut l = line.clone();
                l.remove(i);
                is_safe(&l)
            })
        })
        .count();
    println!("{y}");
}


fn is_safe(line: &[usize]) -> bool {
    line.windows(2).all(|s| {
        let diff = s[0].abs_diff(s[1]);
        (1..=3).contains(&diff)
    }) && (
        line.windows(2).all(|s| s[0] <= s[1]) || line.windows(2).all(|s| s[0] >= s[1])
    )
}