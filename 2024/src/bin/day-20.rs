use std::cmp::Reverse;
use std::collections::hash_map::Entry;
use std::collections::{BTreeMap, BinaryHeap, HashMap, HashSet, VecDeque};

fn main() {
    let input = std::fs::read_to_string("inputs/20.txt").unwrap();
    soln(&input);
}

type Position = (usize, usize);
type PosDelta = (isize, isize);
type State = (Position, Cheats);
#[derive(Default, Clone, Copy, PartialEq, Eq, Hash, Debug, PartialOrd, Ord)]
enum Cheats {
    #[default]
    Z,
    O(Position),
    T(Position, Position)
}

fn translate((r, c): Position, (dr, dc): PosDelta) -> Position {
    (r.wrapping_add_signed(dr), c.wrapping_add_signed(dc))
}
#[derive(Debug)]
struct Grid<T> {
    grid: Vec<Vec<T>>
}
impl<T> Grid<T> {
    fn get(&self, (r, c): Position) -> Option<&T> {
        self.grid.get(r)?.get(c)
    }
}
impl Grid<bool> {
    fn neighbors(&self, p: Position) -> impl Iterator<Item=Position> + '_ {
        [(0, 1), (1, 0), (0, -1), (-1, 0)]
            .into_iter()
            .map(move |d| translate(p, d))
    }
}
struct Data {
    grid: Grid<bool>,
    start: State,
    end: Position
}

fn parse(input: &str) -> Data {
    let mut start = None;
    let mut end = None;

    let grid = input.lines().enumerate()
        .map(|(r, l)| l.bytes().enumerate().map(|(c, b)| match b {
            b'#' => true,
            b'.' => false,
            b'S' => { start.replace((r, c)); false },
            b'E' => { end.replace((r, c)); false },
            b => unreachable!("{}", char::from(b))
        }).collect())
        .collect();

    Data { grid: Grid { grid }, start: (start.unwrap(), Cheats::Z), end: end.unwrap() }
}
fn manhattan((r0, c0): Position, (r1, c1): Position) -> i32 {
    (r1.abs_diff(r0) + c1.abs_diff(c0)) as i32
}

fn soln(input: &str) {
    let Data { grid, start, end } = parse(input);

    let mut dist_map: HashMap<_, _> = HashMap::from_iter([(start.0, 0)]);
    let mut queue = VecDeque::from([start.0]);
    while let Some(p) = queue.pop_front() {
        if p == end { break; }

        let d = dist_map[&p];
        grid.neighbors(p)
            .filter(|&np| grid.get(np).is_some_and(|&n| !n))
            .for_each(|np| if let Entry::Vacant(e) = dist_map.entry(np) {
                e.insert(d + 1);
                queue.push_back(np);
            });
    }
    // let end_time = dist_map[&end];
    let portal_time = |p0, p1, pdist| p1 - p0 - pdist;
    // let walls: Vec<_> = grid.grid.iter().enumerate()
    //     .flat_map(|(r, row)| {
    //         row.iter().enumerate()
    //             .filter(|&(_, &b)| b)
    //             .map(move |(c, _)| (r, c))
    //     })
    //     .filter_map(|p| {
    //         let connectors: Vec<_> = grid.neighbors(p)
    //             .filter_map(|np| dist_map.get(&np).copied())
    //             .collect();

    //         (connectors.len() >= 2).then_some((p, connectors))
    //     })
    //     .map(|(p, mut ctr)| {
    //         ctr.sort();
    //         let ctrref = &ctr;
    //         let portals: Vec<_> = (0..ctr.len()).flat_map(|i| {
    //             ((i + 1)..ctr.len()).map(move |j| (ctrref[i], ctrref[j]))
    //         })
    //         .filter(|&portal| portal_time(portal) > 0)
    //         .collect();

    //         (p, portals)
    //     })
    //     .collect();
    
    // let mut time_saved = vec![];
    // for i in 0..walls.len() {
    //     for j in (i + 1)..=walls.len() {
    //         let (_, lportals) = &walls[i];
    //         match walls.get(j) {
    //             Some((_, rportals)) => {
    //                 time_saved.extend({
    //                     lportals.iter().flat_map(|&lportal @ (l0, l1)| {
    //                         rportals.iter()
    //                             .filter(move |&&(r0, r1)| l1 <= r0 && r1 <= l0) // check disjoint ranges
    //                             .map(move |&rportal| portal_time(lportal) + portal_time(rportal))
    //                     })
    //                 });
    //             },
    //             None => {
    //                 time_saved.extend({
    //                     lportals.iter().map(|&portal| portal_time(portal))
    //                 })
    //             }
    //         }
    //     }
    // }

    // let p1 = time_saved.iter().filter(|&&n| n >= 100).count();
    // println!("{p1}");
    
    let mut spaces: Vec<_> = dist_map.keys().copied().collect();
    spaces.sort_by_key(|p| dist_map[p]);

    let mut valid_portals = vec![];
    for (i, &pi) in spaces.iter().enumerate() {
        for &pj in &spaces[(i + 1)..] {
            let dist = manhattan(pi, pj);
            let di = dist_map[&pi];
            let dj = dist_map[&pj];
            let pt = portal_time(di, dj, dist);
            if dist <= 20 && pt > 0 {
                valid_portals.push((di, dj, dist, pt));
            }
        }
    }
    // for i in 0..spaces.len() {
    //     for j in (i + 1)..spaces.len() {
    //         let dist = manhattan(spaces[i], spaces[j]);
    //         let pi = dist_map[&spaces[i]];
    //         let pj = dist_map[&spaces[j]];
    //         let pt = portal_time((pi, pj, dist));
    //         if dist == 2 && portal_time((pi, pj, dist)) > 0 {
    //             valid_portals.push((pi, pj, dist, pt));
    //         }
    //     }
    // };
    // valid_portals.sort_by_key(|&(_, _, _, pt)| pt);
    // println!("{:?}", valid_portals.last());
    // // println!("length: {:?}", valid_portals.len());
    // // let mut portal_buckets = BTreeMap::from_iter([(0, vec![])]);
    // // for &p in &valid_portals {
    // //     portal_buckets.entry(p.3).or_insert_with(Vec::new).push(p);
    // // }

    // // let mut count = 0;
    // // for (&k, lportals) in portal_buckets.range(..=50) {
    // //     for &lp in lportals {
    // //         for (_, rportals) in portal_buckets.range((100 - k)..) {
    // //             for &rp in rportals {
    // //                 if lp.1 <= rp.0 && lp.0 <= rp.1 {
    // //                     count += 1;
    // //                 }
    // //             }
    // //         }
    // //     }
    // // }
    // let mut time_saved = vec![];
    // for i in 0..valid_portals.len() {
    //     for j in (i + 1)..=valid_portals.len() {
    //         let (l0, l1, _, lt) = valid_portals[i];
    //         match valid_portals.get(j) {
    //             Some(&(r0, r1, _, rt)) => if r1 <= l0 && l1 <= r0 {
    //                 time_saved.push(lt + rt)
    //             },
    //             None => time_saved.push(lt)
    //         };
    //     }
    // }

    // // let mut ctr = HashMap::new();
    // // for &t in &time_saved {
    // //     *ctr.entry(t).or_default() += 1;
    // // }

    // // for i in [50, 52, 54, 56, 58, 60, 62, 64, 66, 68, 70, 72, 74, 76] {
    // //     println!("{i} ps saved: {}", ctr.get(&i).unwrap_or(&0));
    // // }
    let p2 = valid_portals.iter().filter(|&&(_, _, _, pt)| pt >= 100).count();
    // let p2 = time_saved.iter().filter(|&&n| n >= 100).count();
    println!("{p2}");
}