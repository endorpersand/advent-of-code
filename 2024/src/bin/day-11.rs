// use std::collections::HashSet;

use std::collections::HashMap;

fn main() {
    let input = std::fs::read_to_string("inputs/11.txt").unwrap();
    part2(&input);
}

#[allow(dead_code)]
fn part1(input: &str) {
    let mut data: Vec<usize> = input.split_whitespace()
        .map(|s| s.parse())
        .collect::<Result<_, _>>()
        .unwrap();

    for i in 0..25 {
        println!("iter {i}");
        data = data.into_iter()
            .flat_map(|n| match n {
                0 => vec![1],
                n if n.ilog10() % 2 == 1 => {
                    let digits = n.ilog10() + 1;
                    vec![n / 10usize.pow(digits / 2), n % 10usize.pow(digits / 2)]
                }
                n => vec![n * 2024]
            })
            .collect();
    }

    println!("{}", data.len());
}

#[allow(dead_code)]
fn part2(input: &str) {
    let data: Vec<usize> = input.split_whitespace()
        .map(|s| s.parse())
        .collect::<Result<_, _>>()
        .unwrap();

    let mut ctr: HashMap<usize, usize> = data.into_iter()
        .map(|s| (s, 1))
        .collect();

    fn insert(m: &mut HashMap<usize, usize>, keys: &[usize], n: usize) {
        keys.iter().copied().for_each(|k| *m.entry(k).or_default() += n);
    }
    for _ in 0..75 {
        let mut new_ctr = HashMap::new();
        for (n, c) in ctr {
            match n {
                0 => insert(&mut new_ctr, &[1], c),
                n if n.ilog10() % 2 == 1 => {
                    let e = (n.ilog10() + 1) / 2;
                    insert(&mut new_ctr, &[n / 10usize.pow(e), n % 10usize.pow(e)], c)
                }
                n => insert(&mut new_ctr, &[n * 2024], c)
            }
        }
        ctr = new_ctr;
    }

    println!("{}", ctr.values().sum::<usize>());
}