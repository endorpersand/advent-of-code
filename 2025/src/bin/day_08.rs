use disjoint::DisjointSet;

fn main() {
    let input = std::fs::read_to_string("inputs/08.txt").unwrap();
    println!("{}", part1(&input));
    println!("{}", part2(&input));
}

type Vector = [usize; 3];

fn sq_dist(l: Vector, r: Vector) -> usize {
    std::iter::zip(l, r)
        .map(|(a, b)| a.abs_diff(b).pow(2))
        .sum()
}
fn dist_vec(vecs: &[Vector]) -> Vec<(usize, [usize; 2])> {
    let mut distances: Vec<_> = vecs.iter().enumerate()
        .flat_map(|(i, &v)| {
            vecs[i + 1 ..].iter()
                .zip(i + 1 ..)
                .map(move |(&w, j)| (sq_dist(v, w), [i, j]))
        })
        .collect();

    distances.sort();
    distances
}

fn parse(input: &str) -> Vec<[usize; 3]> {
    input.lines()
        .map(|l| {
            let mut it = l.split(',');
            std::array::from_fn(|_| it.next().unwrap().parse().unwrap())
        }).collect()
}
fn part1(input: &str) -> usize {
    const SHORTEST_N: usize = 1000;
    const TOP_N: usize = 3;

    let vectors = parse(input);
    let distances = dist_vec(&vectors);
    let mut jboxes = DisjointSet::with_len(vectors.len());
    
    for &(_, [i, j]) in &distances[0..SHORTEST_N] {
        jboxes.join(i, j);
    }

    let mut sets = jboxes.sets();
    sets.sort_by_key(|s| !s.len());
    sets[0..TOP_N].iter().map(|s| s.len()).product()
}
fn part2(input: &str) -> usize {
    let vectors = parse(input);
    let distances = dist_vec(&vectors);
    let mut jboxes = DisjointSet::with_len(vectors.len());

    fn count_sets(v: &DisjointSet) -> usize {
        (0..v.len())
            .filter(|&i| v.root_of(i) == i)
            .count()
    }

    for (_, [i, j]) in distances {
        if jboxes.join(i, j) && count_sets(&jboxes) == 1 {
            return vectors[i][0] * vectors[j][0];
        }
    }
    panic!("can't find");
}
