use std::cmp;

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

    soln2(&data);
}

#[allow(unused)]
fn soln1(data: &[Vec<usize>]) {
    fn is_safe(line: &[usize]) -> bool {
        line.windows(2).all(|s| {
            let diff = s[0].abs_diff(s[1]);
            (1..=3).contains(&diff)
        }) && (
            line.windows(2).all(|s| s[0] <= s[1]) || line.windows(2).all(|s| s[0] >= s[1])
        )
    }

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

#[allow(unused)]
fn soln2(data: &[Vec<usize>]) {
    // Single pass safe check
    fn is_safe(line: &[usize]) -> bool {
        let mut sign = cmp::Ordering::Equal;
        line.windows(2).all(|s| {
            let monotonic = match (sign, s[0].cmp(&s[1])) {
                (c, cmp::Ordering::Equal) => false,
                (cmp::Ordering::Equal, c) => { sign = c; true },
                (a, b) => a == b
            };
            
            monotonic && s[0].abs_diff(s[1]) <= 3
        })
    }

    let x: usize = data.iter()
        .filter(|line| is_safe(line))
        .count();
    println!("{x}");

    // let y: usize = data.iter()
    //     .filter(|&line| {
    //         let mut sign = 0;
    //         let mut skipped = None;

    //         let mut i = 0;
    //         while i < line.len() - 1 {
    //             let mut l = line[i] as isize;
    //             let r = line[i + 1] as isize;
    //             let mut diff = l - r;
    
    //             if diff == 0 || diff.abs() > 3 {
    //                 if skipped.is_some() { return false; }

    //                 if i == 0 || (line[i - 1] as isize) == r || (line[i - 1] as isize) - r > 3 {
    //                     skipped.replace(i);
    //                     i += 1;
    //                 } else if line.get(i + 2).is_some_and(|&rr| l == rr as isize || l - rr > 3) {
                        
    //                 }
    //                 i += 1;
    //             }

    //             if sign == 0 { sign = diff.signum(); }
    //             if sign != diff.signum() { return false; }

    //             i += 1;
    //         }
            
    //         true
    //     })
    //     .count();
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