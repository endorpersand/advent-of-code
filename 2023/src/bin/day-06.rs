fn main() {
    let txt = std::fs::read_to_string("inputs/06.txt").unwrap();
    let State { time, dist } = parse2(&txt); // j switch between parse and parse2

    let out: usize = std::iter::zip(&time, &dist)
        .map(|(&t, &d)| {
            let t = t as f64;
            let d = d as f64;

            let half_t = t / 2.;
            let discrim = (t.powi(2) - 4. * d).sqrt() / 2.;

            let min = (half_t - discrim).max(0.);
            let max = half_t + discrim;

            ((min + 0.01).ceil() as usize) ..= ((max - 0.01).floor() as usize)
        })
        .map(|c| c.count())
        .product();
    println!("{out}");
}

#[derive(Debug)]
struct State {
    time: Vec<usize>,
    dist: Vec<usize>
}
fn parse(file: &str) -> State {
    let (time_line, dist_line) = file.split_once('\n').unwrap();
    
    let mut time_it = time_line.split_whitespace();
    time_it.next();
    let time = time_it.map(|s| s.parse().unwrap()).collect();
    
    let mut dist_it = dist_line.split_whitespace();
    dist_it.next();
    let dist = dist_it.map(|s| s.parse().unwrap()).collect();

    State { time, dist }
}
fn parse2(file: &str) -> State {
    let (time_line, dist_line) = file.split_once('\n').unwrap();
    
    let mut time_it = time_line.split_whitespace();
    time_it.next();
    let time = time_it.collect::<String>().parse().unwrap();
    
    let mut dist_it = dist_line.split_whitespace();
    dist_it.next();
    let dist = dist_it.collect::<String>().parse().unwrap();

    State { time: vec![time], dist: vec![dist] }
}