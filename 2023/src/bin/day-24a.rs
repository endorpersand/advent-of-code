use std::num::NonZeroIsize;

const MIN: isize = 200_000_000_000_000;
const MAX: isize = 400_000_000_000_000;

fn main() {
    let txt = std::fs::read_to_string("inputs/24.txt").unwrap();
    let State { stones } = parse(&txt);

    let start = MIN as f64;
    let end   = MAX as f64;
    
    let intersects = (0..stones.len())
        .flat_map(|i| ((i + 1)..stones.len()).map(move |j| (i, j)))
        .filter_map(|(i, j)| stones[i].intersects2d(stones[j]))
        .filter(|&([t0, t1], [x, y])| {
            t0 >= 0. && t1 >= 0. && start <= x && x <= end && start <= y && y <= end
        })
        .count();
    println!("{intersects}");
}

#[derive(Debug)]
struct State {
    stones: Vec<Hailstone>
}
fn parse(file: &str) -> State {
    let stones = file.lines()
        .map(|line| {
            let (pos_str, vel_str) = line.split_once(" @ ").unwrap();

            let bpos = pos_str.split(", ")
                .map(|s| s.trim().parse().unwrap())
                .collect::<Box<_>>();
            let bvel = vel_str.split(", ")
                .map(|s| s.trim().parse().unwrap())
                .collect::<Box<_>>();

            Hailstone {
                pos: *<Box<_>>::try_from(bpos).unwrap(),
                vel: *<Box<_>>::try_from(bvel).unwrap(),
            }
        })
        .collect();
    
    State { stones }
}

#[derive(Debug, Clone, Copy)]
struct Hailstone {
    pos: [isize; 3],
    vel: [isize; 3]
}
impl Hailstone {
    fn intersects2d(self, other: Hailstone) -> Option<([f64; 2] /* time */, [f64; 2] /* pos */)> {
        let Hailstone { pos: [x0, y0, _], vel: [vx0, vy0, _] } = self;
        let Hailstone { pos: [x1, y1, _], vel: [vx1, vy1, _] } = other;

        let tn = (y0 - y1) * vx1 - (x0 - x1) * vy1;
        let un = (y0 - y1) * vx0 - (x0 - x1) * vy0;
        let d = NonZeroIsize::new(vx0 * vy1 - vy0 * vx1)?;

        let x = (x0 as f64) + (vx0 as f64 / d.get() as f64) * tn as f64;
        let y = (y0 as f64) + (vy0 as f64 / d.get() as f64) * tn as f64;

        Some((
            [tn as f64 / d.get() as f64, un as f64 / d.get() as f64], 
            [x, y], 
        ))
    }
}