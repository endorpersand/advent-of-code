use std::cmp::Reverse;
use std::collections::HashMap;
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
    //     path: vec![
    //         String::from("AA"),
    //         String::from("DD"),
    //         String::from("BB"),
    //         String::from("JJ"),
    //         String::from("HH"),
    //         String::from("EE"),
    //         String::from("CC"),
    //     ],
    // }.pressure(&m);
    // println!("{pressure_test}");

    let paths = find_paths(&m);

    println!("searching through {} paths", paths.len());

    let optimal = paths.into_iter()
        .map(|p| p.accelerate(30))
        .max_by_key(|p| p.pressure)
        .unwrap();
    
    let it1 = optimal.path.iter();
    let mut it2 = optimal.path.iter();
    it2.next();
    for (a, b) in std::iter::zip(it1, it2) {
        println!("{a} -> {b}, {}, opening: {}", m[a].tunnels[b], m[a].rate)
    }
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

#[derive(Clone, PartialEq, Eq, Debug)]
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

    fn diverge(&self, m: &ValveMap) -> Vec<Path> {
        let current = self.current();
        let valve = &m[current];

        valve.tunnels.iter()
            .filter(|&(tun, _)| !self.path.contains(tun))
            .filter_map(|(tun, time)| {
                let mut p = self.clone();
                // we're going to open this, so add 1 to the time taken
                p.time += time + 1;
                p.pressure += (time + 1) * p.rate;
                p.rate += m[tun].rate as usize;

                p.path.push(tun.clone());

                (p.time <= 30).then_some(p)
            })
            .collect()
    }

    fn accelerate(&self, to: usize) -> Path {
        let mut p = self.clone();
        let dt = to - p.time;

        p.time = to;
        p.pressure += dt * p.rate;

        p
    }
}

fn find_paths(m: &ValveMap) -> Vec<Path> {
    let mut traversed = vec![];
    let mut frontier = vec![Path::new("AA")];

    loop {
        println!("frontier: {}", frontier.len());
        let new_paths: Vec<_> = frontier.iter()
            .flat_map(|p| p.diverge(m))
            .collect();
        traversed.append(&mut frontier);
        
        if new_paths.is_empty() {
            return traversed;
        } else {
            let mut exceeded = vec![];
            let mut continuing = vec![];

            for p in new_paths {
                let vec: &mut _ = if p.time >= 30 {
                    &mut exceeded
                } else {
                    &mut continuing
                };

                vec.push(p);
            }
            traversed.extend(exceeded);
            frontier.extend(continuing);
        }
    }
}