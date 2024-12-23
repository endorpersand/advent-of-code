use std::collections::{HashMap, HashSet};

fn main() {
    let input = std::fs::read_to_string("inputs/23.txt").unwrap();
    soln(&input);
}

fn soln(input: &str) {
    let data: Vec<_> = input.lines()
        .flat_map(|s| s.split_once('-'))
        .collect();

    let mut nodes = HashSet::new();
    let mut edges = HashMap::new();
    let mut tris = vec![];
    for (l, r) in data {
        let le = !nodes.insert(l);
        let re = !nodes.insert(r);
        
        // check if either have existing nodes:
        if le && re {
            tris.extend({
                edges.get(l).map_or::<&[_], _>(&[], Vec::as_slice).iter()
                    .filter(|&&a| edges.get(a).is_some_and(|e| e.contains(&r)))
                    .map(|&a| [l, a, r])
            });
        }
        edges.entry(l).or_insert_with(Vec::new).push(r);
        edges.entry(r).or_insert_with(Vec::new).push(l);
    }

    let p1 = tris.iter()
        .filter(|tri| tri.iter().any(|v| v.starts_with('t')))
        .count();
    println!("{p1}");

    // Finds largest clique of size |adjacents| - 1.
    // This happens to work for the way this day was designed.
    let is_clique = |adj: &[_]| {
        adj.iter().enumerate()
            .all(|(i, n)| adj[i+1..].iter().all(|m| edges[n].contains(m)))
    };
    let mut max_clique = None;
    'outer: for (&n, adj) in &edges {
        let clique = [&[n], &**adj].concat();
        for i in 0..clique.len() {
            let mut clq = clique.clone();
            clq.swap_remove(i);
            if is_clique(&clq) {
                max_clique.replace(clq);
                break 'outer;
            };
        }
    }
    let mut clq = max_clique.unwrap();
    clq.sort();
    println!("{}", clq.join(","));
}