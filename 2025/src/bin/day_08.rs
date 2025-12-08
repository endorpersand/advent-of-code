fn main() {
    let input = std::fs::read_to_string("inputs/08.txt").unwrap();
    println!("{}", part1(&input));
    println!("{}", part2(&input));
}

type Vector = [usize; 3];
struct Dsds {
    parents: Vec<usize>,
    sizes: Vec<usize>
}
impl Dsds {
    fn new(len: usize) -> Self {
        Self { parents: (0..len).collect(), sizes: vec![1; len] }
    }
    fn find(&mut self, mut i: usize) -> usize {
        while self.parents[i] != i {
            (i, self.parents[i]) = (self.parents[i], self.parents[self.parents[i]]);
        }
        i
    }
    fn join(&mut self, i: usize, j: usize) -> bool {
        let i = self.find(i);
        let j = self.find(j);

        if i == j { return false; }
        let [i, j] = if self.sizes[i] <= self.sizes[j] { [i, j] } else { [j, i] };
        self.parents[i] = j;
        self.sizes[j] += self.sizes[i];
        true
    }
    fn roots(&self) -> usize {
        (0..self.parents.len())
            .filter(|&i| i == self.parents[i])
            .count()
    }
    fn set_sizes(&mut self) -> Vec<usize> {
        let mut sz: Vec<_> = (0..self.parents.len())
            .filter(|&i| i == self.parents[i])
            .map(|i| self.sizes[i])
            .collect();
        sz.sort_by_key(|&k| !k);
        sz
    }
}
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
    let mut jboxes = Dsds::new(vectors.len());
    
    for &(_, [i, j]) in &distances[0..SHORTEST_N] {
        jboxes.join(i, j);
    }

    jboxes.set_sizes()[0..TOP_N].iter().product()
}
fn part2(input: &str) -> usize {
    let vectors = parse(input);
    let distances = dist_vec(&vectors);
    let mut jboxes = Dsds::new(vectors.len());

    for (_, [i, j]) in distances {
        if jboxes.join(i, j) && jboxes.roots() == 1 {
            return vectors[i][0] * vectors[j][0];
        }
    }
    panic!("can't find");
}
