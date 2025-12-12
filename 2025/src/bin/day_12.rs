fn main() {
    let input = std::fs::read_to_string("inputs/12.txt").unwrap();
    println!("{}", soln(&input));
}

fn parse(input: &str) -> Vec<(usize, usize, Vec<usize>)> {
    input.lines()
        .skip_while(|l| !l.contains('x'))
        .map(|l| {
            let (dim, ct) = l.split_once(": ").unwrap();
            let (wstr, hstr) = dim.split_once('x').unwrap();
            let width = wstr.parse().unwrap();
            let height = hstr.parse().unwrap();
            let count = ct.split(' ')
                .map(|n| n.parse().unwrap())
                .collect();

            (width, height, count)
        })
        .collect()
}
fn soln(input: &str) -> usize {
    let data = parse(input);
    data.iter()
        .filter(|&(width, height, count)| width * height >= count.iter().sum::<usize>() * 9)
        .count()

}