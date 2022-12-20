use std::fs;
use std::ops::{Index, IndexMut};
use std::str::FromStr;

use regex::Regex;

fn main() {
    let test_input = fs::read_to_string("inputs/test/19.txt").unwrap();
    let test_blueprints: Vec<Blueprint> = test_input.lines().map(|s| s.parse().unwrap()).collect();

    let input = fs::read_to_string("inputs/19.txt").unwrap();
    let blueprints: Vec<Blueprint> = input.lines().map(|s| s.parse().unwrap()).collect();
    
    let s = part_a(&test_blueprints);
    assert_eq!(s[0], 9);
    assert_eq!(s[1], 12);
    part_a(&blueprints);

    let s = part_b(&test_blueprints);
    // assert_eq!(s[0], 56);
    assert_eq!(s[1], 62);
    part_b(&blueprints);
}

fn part_a(bps: &[Blueprint]) -> Vec<usize> {
    let sol: Vec<_> = std::iter::zip(1.., bps)
        .map(|(i, &bp)| {
            let geodes = bfs(bp, 24, 30000).rs.rs[Resource::Geode];
            println!("Blueprint {i}: {geodes} geodes");
            geodes
        })
        .collect();
    
    let s: usize = std::iter::zip(1.., sol.iter()).map(|(i, g)| i * g).sum();
    println!("{s}");

    sol
}

fn part_b(bps: &[Blueprint]) -> Vec<usize> {
    let sol: Vec<_> = std::iter::zip(1.., bps)
        .take(3)
        .map(|(i, &bp)| {
            let geodes = bfs(bp, 32, 20000).rs.rs[Resource::Geode];
            println!("Blueprint {i}: {geodes} geodes");
            geodes
        })
        .collect();
    
    let s: usize = sol.iter().product();
    println!("{s}");

    sol
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Blueprint {
    costs: [[usize; 4]; 4] // first index is machine material, inner array is cost
}
impl FromStr for Blueprint {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let regex = Regex::new(r"Blueprint \d+: Each ore robot costs (\d+) ore. Each clay robot costs (\d+) ore. Each obsidian robot costs (\d+) ore and (\d+) clay. Each geode robot costs (\d+) ore and (\d+) obsidian.")
            .unwrap();
        let captures = regex.captures(s).unwrap();
        
        Ok(Blueprint {
            costs: [
                [captures[1].parse().unwrap(), 0, 0, 0],
                [captures[2].parse().unwrap(), 0, 0, 0],
                [captures[3].parse().unwrap(), captures[4].parse().unwrap(), 0, 0],
                [captures[5].parse().unwrap(), 0, captures[6].parse().unwrap(), 0],
            ]
        })
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Resource {
    Ore   = 0, 
    Clay  = 1, 
    Obs   = 2, 
    Geode = 3
}
const RESOURCES: [Resource; 4] = [Resource::Ore, Resource::Clay, Resource::Obs, Resource::Geode];

impl<T> Index<Resource> for [T; 4] {
    type Output = T;

    fn index(&self, index: Resource) -> &Self::Output {
        &self[index as usize]
    }
}
impl<T> IndexMut<Resource> for [T; 4] {
    fn index_mut(&mut self, index: Resource) -> &mut Self::Output {
        &mut self[index as usize]
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
struct Resources {
    elapsed: usize,
    rs: RsVector,
    machs: RsVector,
    pending_machs: RsVector
}
impl Resources {
    fn new() -> Self {
        Self {
            elapsed: 0,
            rs: [0; 4],
            machs: [1, 0, 0, 0],
            pending_machs: [0; 4]
        }
    }

    fn iterate(&self) -> Self {
        let mut rs = self.clone();
        
        rs.elapsed += 1;

        std::iter::zip(rs.rs.iter_mut(), rs.machs)
            .for_each(|(r, m)| *r += m);
        
        std::iter::zip(rs.machs.iter_mut(), rs.pending_machs)
            .for_each(|(m, p)| *m += p);
        rs.pending_machs.fill(0);

        rs
    }

    fn try_buy(&self, mach: Resource, bp: Blueprint) -> Option<Self> {
        let mut rs = self.clone();
        
        rs.rs = zip_sub(rs.rs, bp.costs[mach])?;
        rs.pending_machs[mach] += 1;
        
        Some(rs)
    }

    fn try_buy_vector(&self, bv: RsVector, bp: Blueprint) -> Option<Self> {
        let mut rs = self.clone();
        for (c, m) in std::iter::zip(bv, RESOURCES) {
            for _ in 0..c { rs = rs.try_buy(m, bp)?; }
        }

        Some(rs)
    }

    fn meets_cap(&self, caps: RsVector) -> bool {
        std::iter::zip(self.machs, caps).take(3).all(|(a, b)| a <= b)
    }
}

type RsVector = [usize; 4];

fn incr(a: [usize; 4], rs: Resource) -> [usize; 4]
{
    let mut b = a;
    b[rs] += 1;
    b
}
fn zip_sub<const N: usize>(a: [usize; N], b: [usize; N]) -> Option<[usize; N]> {
    std::iter::zip(a, b)
        .map(|(a1, b1)| a1.checked_sub(b1))
        .collect::<Option<Vec<_>>>()?
        .try_into()
        .ok()
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Path {
    blueprint: Blueprint,
    path: Vec<RsVector>,
    rs: Resources,
}
impl Path {
    fn new(bp: Blueprint) -> Self {
        Self { blueprint: bp, path: vec![], rs: Resources::new() }
    }

    fn pushed(&self, bv: RsVector) -> Option<Path> {
        let mut p = self.clone();
        
        p.rs = p.rs.try_buy_vector(bv, p.blueprint)?.iterate();
        p.path.push(bv);

        Some(p)
    }

    fn branch(&self, mach: impl Into<Option<Resource>>, caps: RsVector) -> Option<Path> {
        let vec = mach.into().map_or([0; 4], |m| incr([0; 4], m));
        self.pushed(vec)
            .filter(|p| p.rs.meets_cap(caps))
    }

    fn branches(&self) -> Vec<Path> {
        // don't buy more than the max cost necessary
        let caps = self.blueprint.costs.into_iter()
            .reduce(|acc, cv| {
                std::iter::zip(acc, cv)
                    .map(|(a, b)| a.max(b))
                    .collect::<Vec<_>>()
                    .try_into()
                    .unwrap()
            })
            .unwrap();

        // always go for geo/obs
        if let Some(br) = self.branch(Resource::Geode, caps) { vec![br] }
        else if let Some(br) = self.branch(Resource::Obs, caps) { vec![br] }
        else {
            [Some(Resource::Clay), Some(Resource::Ore), None]
                .into_iter()
                .filter_map(|m| self.branch(m, caps))
                .collect()
        }
    }
}

fn bfs(bp: Blueprint, len: usize, pop: usize) -> Path {
    // type EvalPath
    let mut frontier = vec![Path::new(bp)];

    fn fitness(p: &Path, term: usize) -> [usize; 4] {
        let rs = &p.rs;
        [
            rs.rs[Resource::Geode] + rs.machs[Resource::Geode] * (term - p.rs.elapsed),
            rs.machs[Resource::Obs],
            rs.machs[Resource::Clay],
            rs.machs[Resource::Ore],
        ]
    }

    for i in 0..len {
        let mut evaluated: Vec<_> = frontier.drain(..)
            .flat_map(|p| {
                p.branches()
            })
            .collect();
        evaluated.sort_unstable_by_key(|p| fitness(p, len));

        println!("iter {i}, evaluated {}, {:?}..={:?}", evaluated.len(), fitness(evaluated.first().unwrap(), len), fitness(evaluated.last().unwrap(), len));
        // keep top n 
        let top = evaluated.drain(..)
            .rev()
            .take(pop);
        frontier.extend(top);
    }

    frontier.swap_remove(0)
}