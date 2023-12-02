// This is a revised version of my day 2 code.
// See original solution in day-02.rs.

use std::ops::Add;

use once_cell::sync::Lazy;
use regex::Regex;

fn main() {
    const LIMIT: CubeCounter = CubeCounter([12, 13, 14]);
    let txt = std::fs::read_to_string("inputs/02.txt").unwrap();

    let out: usize = txt.lines()
        .zip(1..)
        .filter(|(line, _)| count_game(line).into_iter().all(|c| c.accepted_by(LIMIT)))
        .map(|(_, i)| i)
        .sum();
    println!("{out:?}");
    
    let out2: usize = txt.lines()
        .map(|line| {
            let cts = count_game(line);
            cts.into_iter().fold(CubeCounter::default(), CubeCounter::as_max)
        })
        .map(|CubeCounter([r, g, b])| r * g * b)
        .sum();
    println!("{out2:?}");
        
}

fn count_game(s: &str) -> Vec<CubeCounter> {
    static RE: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"(\d+) (red|green|blue)").unwrap()
    });

    let (_, rest) = s.split_once(": ").unwrap();
    rest.split("; ")
        .map(|s| {
            let mut ctr = [0; 3];
            for capture in RE.captures_iter(s) {
                let ct: usize = capture[1].parse().unwrap();
                let idx = match &capture[2] {
                    "red" => 0,
                    "green" => 1,
                    "blue" => 2,
                    _ => unreachable!()
                };

                ctr[idx] += ct;
            }

            CubeCounter(ctr)
        })
        .collect()
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