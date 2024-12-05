use std::collections::HashSet;

fn main() {
    let input = std::fs::read_to_string("inputs/05.txt").unwrap();
    soln1(&input);
}

#[allow(dead_code)]
fn soln1(input: &str) {
    let mut lines = input.lines();
    let rules: Vec<(usize, usize)> = lines.by_ref()
        .take_while(|s| !s.is_empty())
        .map(|s| {
            let (ls, rs) = s.split_once('|').unwrap();
            (ls.parse().unwrap(), rs.parse().unwrap())
        })
        .collect();

    // let p1 = lines.map(|s| {
    //         s.split(',')
    //             .map(|seg| seg.parse().unwrap())
    //             .collect::<Vec<usize>>()
    //     })
    //     .filter(|group| {
    //         rules.iter().all(|(l, r)| {
    //             let left = group.iter().position(|n| n == l);
    //             let right = group.iter().position(|n| n == r);

    //             left.zip(right).is_none_or(|(li, ri)| li < ri)
    //         })
    //     })
    //     .map(|group| group[group.len() / 2])
    //     .sum::<usize>();
    // println!("{p1}");

    let rule_set: HashSet<_> = rules.iter().copied().collect();
    let p2 = lines.map(|s| {
        s.split(',')
            .map(|seg| seg.parse().unwrap())
            .collect::<Vec<usize>>()
    })
        .filter(|group| {
            !rules.iter().all(|(l, r)| {
                let left = group.iter().position(|n| n == l);
                let right = group.iter().position(|n| n == r);

                left.zip(right).is_none_or(|(li, ri)| li < ri)
            })
        })
        .map(|mut group| {
            let mut sorted = vec![];

            while !group.is_empty() {
                let (next_index, _) = group.iter().enumerate()
                    .find(|&(i, &n)| {
                        group[..i].iter().chain(&group[i+1..])
                            .all(|&m| !rule_set.contains(&(m, n)))
                    })
                    .unwrap();
                sorted.push(group.swap_remove(next_index));
            }

            sorted
        })
        .inspect(|g| println!("{g:?}"))
        .map(|group| group[group.len() / 2])
        .sum::<usize>();

    println!("{p2}");
}

#[allow(dead_code)]
fn soln2(input: &str) {
    let mut lines = input.lines();
    let rules: HashSet<(usize, usize)> = lines.by_ref()
        .take_while(|s| !s.is_empty())
        .map(|s| {
            let (ls, rs) = s.split_once('|').unwrap();
            (ls.parse().unwrap(), rs.parse().unwrap())
        })
        .collect();

    let (ordered, mut malordered): (Vec<_>, _) = lines.map(|s| {
            s.split(',')
                .map(|seg| seg.parse().unwrap())
                .collect::<Vec<usize>>()
        })
        .partition(|group| rules.iter().all(|(l, r)| {
            let left = group.iter().position(|n| n == l);
            let right = group.iter().position(|n| n == r);

            left.zip(right).is_none_or(|(li, ri)| li < ri)
        }));
    
    let p1 = ordered.iter()
        .map(|g| g[g.len() / 2])
        .sum::<usize>();
    println!("{p1}");

    let p2 = malordered.iter_mut()
        .map(|group| {
            std::iter::from_fn(|| {
                group.iter().enumerate().position(|(i, &n)| {
                    Iterator::chain(group[..i].iter(), &group[i + 1..])
                        .all(|&m| !rules.contains(&(m, n)))
                })
                .map(|i| group.swap_remove(i))
            }).collect::<Vec<_>>()
        })
        .map(|group| group[group.len() / 2])
        .sum::<usize>();

    println!("{p2}");
}