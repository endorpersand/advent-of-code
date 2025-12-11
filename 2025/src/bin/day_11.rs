use std::collections::HashMap;

fn main() {
    let input = std::fs::read_to_string("inputs/11.txt").unwrap();
    println!("{}", part1(&input));
    println!("{}", part2(&input));
}

struct Graph<'s> {
    nodes: HashMap<&'s str, Vec<&'s str>>
}
fn parse(input: &str) -> Graph<'_> {
    let nodes = input.lines()
        .map(|l| {
            let (n, e) = l.split_once(": ").unwrap();
            let ee = e.split(' ').collect();
            (n, ee)
        })
        .collect();

    Graph { nodes }
}

impl Graph<'_> {
    fn count_paths(&self, from: &str, to: &str) -> usize {
        let mut frontier = vec![from];
        let mut found = 0;
        
        while let Some(node) = frontier.pop() {
            if node == to {
                found += 1;
                continue;
            }
            frontier.extend_from_slice(&self.nodes[node]);
        }

        found
    }

    fn count_paths2<'a>(&'a self, from: &'a str, to: &'a str, memo: &mut HashMap<(&'a str, &'a str), usize>) -> usize {
        if from == to {
            return 1;
        }
        if from == "out" {
            return 0;
        }
        if let Some(&n) = memo.get(&(from, to)) {
            return n;
        }
        
        let result = self.nodes[from].iter()
            .map(|&node| self.count_paths2(node, to, memo))
            .sum();
        memo.insert((from, to), result);

        result
    }
}

fn part1(input: &str) -> usize {
    parse(input).count_paths("you", "out")
}
fn part2(input: &str) -> usize {
    let graph = parse(input);
    let mut memo = Default::default();

    graph.count_paths2("svr", "fft", &mut memo)
    * graph.count_paths2("fft", "dac", &mut memo)
    * graph.count_paths2("dac", "out", &mut memo)
}