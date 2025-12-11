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

type MemoEntry<'a> = (&'a str, bool, bool);
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

    fn count2_memo<'a>(&'a self, entry: MemoEntry<'a>, to: &str, memo: &mut HashMap<MemoEntry<'a>, usize>) -> usize {
        let (from, thru_dac, thru_fft) = entry;
        if from == to {
            return usize::from(thru_dac && thru_fft);
        }
        if let Some(&n) = memo.get(&entry) {
            return n;
        }
        
        let result = self.nodes[from].iter()
            .map(|&node| match node {
                "dac" => self.count2_memo((node, true, thru_fft), to, memo),
                "fft" => self.count2_memo((node, thru_dac, true), to, memo),
                _ => self.count2_memo((node, thru_dac, thru_fft), to, memo)
            })
            .sum();
        memo.insert(entry, result);

        result
    }
}

fn part1(input: &str) -> usize {
    parse(input).count_paths("you", "out")
}
fn part2(input: &str) -> usize {
    parse(input)
        .count2_memo(
            ("svr", false, false),
            "out",
            &mut Default::default()
        )
}