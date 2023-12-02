use std::fs;
use std::ops::Add;

use logos::Logos;

fn main() {
    const LIMIT: CubeCounter = CubeCounter([12, 13, 14]);
    let txt = fs::read_to_string("inputs/02.txt").unwrap();

    let out: usize = txt.lines()
        .zip(1..)
        .filter(|(line, _)| count(line).into_iter().all(|c| c.accepted_by(LIMIT)))
        .map(|(_, i)| i)
        .sum();
    println!("{out:?}");
    
    let out2: usize = txt.lines()
        .map(|line| {
            let cts = count(line);
            cts.into_iter().fold(CubeCounter::default(), CubeCounter::as_max)
        })
        .map(|CubeCounter([r, g, b])| r * g * b)
        .sum();
    println!("{out2:?}");
        
}

#[derive(Logos, Debug, Clone, Copy, PartialEq, Eq)]
enum Cubes {
    #[regex(r"\d+ red", |lx| lx.slice().strip_suffix(" red").unwrap().parse::<usize>().unwrap())]
    Red(usize),
    #[regex(r"\d+ green", |lx| lx.slice().strip_suffix(" green").unwrap().parse::<usize>().unwrap())]
    Green(usize),
    #[regex(r"\d+ blue", |lx| lx.slice().strip_suffix(" blue").unwrap().parse::<usize>().unwrap())]
    Blue(usize),
    #[token(";")]
    Semi
}

impl From<Cubes> for CubeCounter {
    fn from(value: Cubes) -> Self {
        Self(match value {
            Cubes::Red(r) => [r, 0, 0],
            Cubes::Green(g) => [0, g, 0],
            Cubes::Blue(b) => [0, 0, b],
            Cubes::Semi => panic!("can't")
        })
    }
}
#[derive(Debug, Default, Clone, Copy)]
struct CubeCounter([usize; 3]);
impl CubeCounter {
    fn accepted_by(self, limit: CubeCounter) -> bool {
        let [sr, sg, sb] = self.0;
        let [lr, lg, lb] = limit.0;
        (sr <= lr) && (sg <= lg) && (sb <= lb)
    }
    fn as_max(self, limit: CubeCounter) -> CubeCounter {
        let [sr, sg, sb] = self.0;
        let [lr, lg, lb] = limit.0;
        CubeCounter([sr.max(lr), sg.max(lg), sb.max(lb)])
    }
}
impl Add for CubeCounter {
    type Output = CubeCounter;

    fn add(self, rhs: Self) -> Self::Output {
        let CubeCounter([r0, g0, b0]) = self;
        let CubeCounter([r1, g1, b1]) = rhs;
        CubeCounter([r0 + r1, g0 + g1, b0 + b1])
    }
}
fn count(s: &str) -> Vec<CubeCounter> {
    let cubes = Cubes::lexer(s)
        .filter_map(|t| t.ok())
        .collect::<Vec<_>>();

    cubes.split(|t| t == &Cubes::Semi)
        .map(|sl| {
            sl.iter().copied()
                .map(CubeCounter::from)
                .fold(Default::default(), |acc, cv| acc + cv)
        })
        .collect()
}