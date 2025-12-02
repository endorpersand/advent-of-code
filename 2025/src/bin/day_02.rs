use std::collections::{HashSet};
use std::num::ParseIntError;

fn main() {
    let input = std::fs::read_to_string("inputs/02.txt").unwrap();
    soln(&input);
}

fn parse(input: &str) -> Vec<(u64, u64)> {
    input.split(",")
        .filter_map(|i| i.split_once("-"))
        .map(|(l, r)| Ok::<_, ParseIntError>((l.parse()?, r.parse()?)))
        .collect::<Result<_, _>>()
        .unwrap()
}
fn soln(input: &str) {
    let ranges = parse(input);

    // part 1
    let mut invalid_ids = HashSet::new();
    for &(left, right) in &ranges {
        let lsize = left.ilog10() + 1;
        let rsize = right.ilog10() + 1;
        let n = (lsize % 2 == 0).then_some(lsize)
            .or((rsize % 2 == 0).then_some(rsize));

        if let Some(n) = n {
            let mult101 = 10u64.pow(n / 2) + 1;

            let minm = std::cmp::max(left / mult101, 10u64.pow(n / 2 - 1));
            let maxm = mult101 - 1;
            
            invalid_ids.extend({
                (minm..maxm).map(|i| mult101 * i)
                    .skip_while(|&v| v < left)
                    .take_while(|&v| v <= right)
            });
        }
    }

    println!("{}", invalid_ids.iter().sum::<u64>());
    
    // part 2
    let mut invalid_ids = HashSet::new();
    for (left, right) in ranges {
        let lsize = left.to_string().len();
        let rsize = right.to_string().len();
        assert!(rsize.abs_diff(lsize) <= 1);
        
        for n_reps in 1..=(rsize / 2) {
            let n_reps = n_reps as u32;
            invalid_ids.extend({
                (10u64.pow(n_reps - 1) .. 10u64.pow(n_reps))
                    .flat_map(|i| {
                        let lstr = i.to_string().repeat(lsize / n_reps as usize);
                        let rstr = i.to_string().repeat(rsize / n_reps as usize);

                        [(lsize >= 2).then_some(lstr), (rsize >= 2).then_some(rstr)]
                    })
                    .flatten()
                    .map(|s| s.parse::<u64>().unwrap()) // get all dupes
                    .filter(|i| (left..=right).contains(i))
            });
        }
    }

    println!("{}", invalid_ids.iter().sum::<u64>());
}