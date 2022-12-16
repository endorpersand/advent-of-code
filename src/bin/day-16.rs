use std::cmp::Reverse;
use std::collections::{HashMap, BTreeSet};
use std::fs;
use std::str::FromStr;

fn main() {
    let input = fs::read_to_string("inputs/16.txt").unwrap();
    let mut m: ValveMap = input.lines()
        .map(|l| l.parse::<Pair<_, _>>().unwrap())
        .map(|Pair (k, v)| (k, v))
        .collect();
    reduce_valve_map(&mut m);
    println!("{:?}", m);

    // let pressure_test = Path {
    //     time: 0,
    //     pressure: 0,
    //     rate: 0,
    //     path: vec![
    //         String::from("AA"),
    // ]}
    //     .append(&m, "DD")
    //     .append(&m, "BB")
    //     .append(&m, "JJ")
    //     .append(&m, "HH")
    //     .append(&m, "EE")
    //     .append(&m, "CC");
    // println!("{}", pressure_test.pressure);

    // part A
    let paths = find_paths1(&m, 30);

    println!("searching through {} paths", paths.len());

    let optimal = paths.into_iter()
        .map(|p| p.accelerate(30))
        .max_by_key(|p| p.pressure)
        .unwrap();
    
    println!("{:?}", optimal);

    // part B
    let paths = find_bipaths(&m, 26);

    println!("searching through {} paths", paths.len());

    let optimal = paths.into_iter()
        .map(|(p, q)| (p.accelerate(26), q.accelerate(26)))
        .max_by_key(|(p, q)| p.pressure + q.pressure)
        .unwrap();
    
    println!("{:?}", optimal);
}

type ValveMap = HashMap<String, Valve>;
type TunnelMap = HashMap<String, usize /* distance */>;
#[derive(Debug)]
struct Valve {
    rate: isize,
    tunnels: TunnelMap
}

struct Pair<K, V>(K, V);
impl FromStr for Pair<String, Valve> {
    type Err = usize;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let s = s.strip_prefix("Valve ").ok_or(0usize)?;
        let (name, s) = s.split_once(" has flow rate=").ok_or(1usize)?;
        let (frs, valves) = s.split_once("; tunnels lead to valves ")
            .or_else(|| s.split_once("; tunnel leads to valve "))
            .ok_or(2usize)?;

        Ok(Pair(name.to_string(), Valve {
            rate: frs.parse().map_err(|_| 3usize)?,
            tunnels: valves.split(", ").map(|s| (s.to_owned(), 1)).collect()
        }))
    }
}

fn reduce_valve_map(m: &mut ValveMap) {
    // Remove all 0s except for AA
    let remove_keys: Vec<_> = m.iter()
        .filter_map(|(k, v)| (k != "AA" && v.rate == 0).then(|| k.clone()))
        .collect();
    
    for k1 in remove_keys {
        let v = m.remove(&k1).unwrap();
        for (k2, valve) in m.iter_mut().filter(|(_, valve)| valve.tunnels.contains_key(&k1)) {
            // pop the tunnel and push the tunnel distances
            let d1 = valve.tunnels.remove(&k1).unwrap();
            valve.tunnels.extend(
                v.tunnels.iter()
                 .filter(|&(t, _)| t != k2)
                 .map(|(t, d2)| (t.clone(), d1 + d2))
            );
        }
    }

    // Apply Dijkstra's everywhere
    let tunnels: HashMap<_, _> = m.keys()
        .map(|k| (k.clone(), dijkstra(m, k)))
        .collect();
    
    for (tun, nbs) in tunnels {
        let valve = m.get_mut(&tun).unwrap();
        valve.tunnels = nbs;
    }
    
}

fn dijkstra(m: &ValveMap, node: &str) -> TunnelMap {
    // binary heap doesn't have contains, mut
    // assert frontier is sorted
    let mut frontier = vec![];
    let mut expanded = HashMap::new();
    frontier.push((Reverse(0), node.to_string()));
    
    while let Some((Reverse(d), tun)) = frontier.pop() {
        expanded.insert(tun.clone(), d);

        let valve = &m[&tun];
        for (nb, d2) in &valve.tunnels {
            let m_frnode = frontier.iter_mut().find(|(_, s)| s == nb);
            let dist = d + d2;

            if !expanded.contains_key(nb) && m_frnode.is_none() {
                frontier.push((Reverse(dist), nb.to_string()));
            } else if let Some((Reverse(frval), _)) = m_frnode {
                *frval = (*frval).min(dist);
            }
        }
        frontier.sort_unstable();
    }

    expanded.remove(node);
    expanded
}

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
struct Path {
    path: Vec<String>,
    time: usize,
    pressure: usize,
    rate: usize,
}

impl Path {
    fn new(start_node: &str) -> Self {
        Self { path: vec![start_node.to_string()], time: 0, pressure: 0, rate: 0 }
    }

    fn current(&self) -> &str {
        self.path.last().unwrap()
    }

    fn diverge(&self, m: &ValveMap, cap: usize) -> Vec<Path> {
        let current = self.current();
        let valve = &m[current];

        valve.tunnels.keys()
            .filter(|&tun| !self.path.contains(tun))
            .filter_map(|tun| {
                let p = self.append(m, tun);
                (p.time <= cap).then_some(p)
            })
            .collect()
    }
    fn diverge_limited(&self, m: &ValveMap, only_in: &BTreeSet<String>, cap: usize) -> Vec<Path> {
        let current = self.current();
        let valve = &m[current];

        valve.tunnels.keys()
            .filter(|&tun| !self.path.contains(tun) && only_in.contains(tun))
            .filter_map(|tun| {
                let p = self.append(m, tun);
                (p.time <= cap).then_some(p)
            })
            .collect()
    }

    fn append(&self, m: &ValveMap, tun: &str) -> Path {
        let mut p = self.clone();
        let time = m[self.current()].tunnels[tun];

        // assume we're opening, add 1 to time taken
        p.time += time + 1;
        p.pressure += (time + 1) * p.rate;
        p.rate += m[tun].rate as usize;

        p.path.push(tun.to_string());

        p
    }

    fn accelerate(&self, to: usize) -> Path {
        let mut p = self.clone();
        let dt = to - p.time;

        p.time = to;
        p.pressure += dt * p.rate;

        p
    }
}

fn find_paths(m: &ValveMap, cap: usize) -> Vec<Path> {
    let mut traversed = vec![];
    let mut frontier = vec![Path::new("AA")];

    loop {
        println!("frontier: {}", frontier.len());
        let (exceeded, continuing): (Vec<_>, _) = frontier.iter()
            .flat_map(|p| p.diverge(m, cap))
            .partition(|p| p.time >= cap);
        traversed.append(&mut frontier);
        traversed.extend(exceeded);

        if continuing.is_empty() {
            return traversed;
        } else {
            frontier.extend(continuing);
        }
    }
}
fn find_paths1(m: &ValveMap, cap: usize) -> Vec<Path> {
    let mut exhausted = vec![];
    let mut frontier = vec![Path::new("AA")];

    while !frontier.is_empty() {
        println!("frontier: {}", frontier.len());
        let mut new_frontier = vec![];
        while let Some(p) = frontier.pop() {
            let (exceeded, continuing): (Vec<_>, _) = p.diverge(m, cap)
                .into_iter()
                .partition(|p| p.time >= cap);
            
            if exceeded.len() + continuing.len() == 0 {
                exhausted.push(p);
            } else {
                exhausted.extend(exceeded);
                new_frontier.extend(continuing);
            }
        }
        frontier.extend(new_frontier);
    }

    exhausted
}

fn leftovers(m: &ValveMap, p: Path) -> BTreeSet<String> {
    let mut nodes: BTreeSet<_> = m.keys().cloned().collect();
    for n in &p.path { nodes.remove(n); }

    return nodes;
}

fn find_limited_paths(m: &ValveMap, allowed_nodes: &BTreeSet<String>, cap: usize) -> Vec<Path> {
    let mut exhausted = vec![];
    let mut frontier = vec![Path::new("AA")];

    while !frontier.is_empty() {
        let mut new_frontier = vec![];
        while let Some(p) = frontier.pop() {
            let (exceeded, continuing): (Vec<_>, _) = p.diverge_limited(m, allowed_nodes, cap)
                .into_iter()
                .partition(|p| p.time >= cap);
            
            if exceeded.len() + continuing.len() == 0 {
                exhausted.push(p);
            } else {
                exhausted.extend(exceeded);
                new_frontier.extend(continuing);
            }
        }
        frontier.extend(new_frontier);
    }

    exhausted
}

fn find_bipaths(m: &ValveMap, cap: usize) -> Vec<(Path, Path)> {
    let human_paths = find_paths(m, cap);
    println!("{}", human_paths.len());

    let mut memo: HashMap<_, _> = HashMap::new();
    // println!("{:?}", human_paths.iter().map(|p| leftovers(m, p.clone())).collect::<HashSet<_>>().len());
    let bipaths: Vec<_> = human_paths.into_iter()
        .enumerate()
        .map(|(i, p)| { 
            if i % 1000 == 0 {
                println!("evaluated: {}", i)
            }

            p
        })
        .flat_map(|p| {
            let leftovers = leftovers(m, p.clone());

            let mq = memo.entry(leftovers).or_insert_with_key(|k| {
                find_limited_paths(m, k, cap)
                    .into_iter()
                    .map(|p| p.accelerate(cap))
                    .max_by_key(|p| p.pressure)
            })
            .as_ref()
            .cloned();

            mq.map(|q| (p, q))
        })
        .collect();
    
    bipaths
}