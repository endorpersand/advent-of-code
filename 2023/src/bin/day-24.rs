use std::num::NonZeroIsize;

const MIN: isize = 200_000_000_000_000;
const MAX: isize = 400_000_000_000_000;
// const MIN: isize = 7;
// const MAX: isize = 27;

fn main() {
    let txt = std::fs::read_to_string("inputs/24.txt").unwrap();
    let State { stones } = parse(&txt);

    let start = MIN as f64;
    let end   = MAX as f64;
    
    // PART A
    let intersects = (0..stones.len())
        .flat_map(|i| ((i + 1)..stones.len()).map(move |j| (i, j)))
        .filter_map(|(i, j)| stones[i].intersect2d(stones[j]))
        .filter(|&([t0, t1], [x, y])| {
            t0 >= 0. && t1 >= 0. && start <= x && x <= end && start <= y && y <= end
        })
        .count();
    println!("{intersects}");

    // PART B
    // Take 3 hailstones :D
    let [s0, s1, s2] = stones[..3] else { unreachable!() };

    // Iterate through possible 2D velocities for the rock
    for rv2 in Lattice2Iter::new() {
        // Shift the velocities of the hailstone by the potential vel of the rock.
        let s0s = s0.shift_vel2(rv2);
        let s1s = s1.shift_vel2(rv2);
        let s2s = s2.shift_vel2(rv2);

        // Find the intersction point
        // If all 3 intersect at the same position,
        // this is a likely vel (of rock), initial pos (of rock), time of intersection.
        let s01i = s0s.intersect2d(s1s);
        let s02i = s0s.intersect2d(s2s);
        
        if s01i.zip(s02i).is_some_and(|((_, p1), (_, p2))| p1 == p2) {
            // println!("{rv2:?}:");
            
            // let ([t0, t1], p) = s01i.unwrap();
            // let ([_,  t2], _) = s02i.unwrap();
            // println!("rock's 2d position: {p:?}");
            // println!("intersect time for {s0:?}: {t0:?}");
            // println!("intersect time for {s1:?}: {t1:?}");
            // println!("intersect time for {s2:?}: {t2:?}");

            // finding z
            // zr + t0 vzr = zh0 + t0 vzh0
            // zr + t1 vzr = zh1 + t1 vzh1
            let ([t0, t1], [x, y]) = s01i.unwrap();
            let z0 = s0.pos[2] as f64 + t0 * s0.vel[2] as f64;
            let z1 = s1.pos[2] as f64 + t1 * s1.vel[2] as f64;

            let rvz = (z1 - z0) / (t1 - t0);
            let z = z0 - t0 * rvz;
            // println!("rock's z position: {z}");
            // println!("rock's z velocity: {rvz}");

            println!("{}", x + y + z);
            break;
        }
    }
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
    fn intersect2d(self, other: Hailstone) -> Option<([f64; 2] /* time */, [f64; 2] /* pos */)> {
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

// PART B
struct Lattice2Iter {
    mag: usize,
    last: [isize; 2],
    delta: [isize; 2]
}
impl Lattice2Iter {
    fn new() -> Self {
        Lattice2Iter {
            mag: 0,
            last: [0; 2],
            delta: [0; 2]
        }
    }
}

impl Iterator for Lattice2Iter {
    type Item = [isize; 2];

    fn next(&mut self) -> Option<Self::Item> {
        let mag = self.mag as isize;

        if self.last == [0, 0] {
            // do nothing
        } else if self.last == [mag, 0] {
            self.delta = [-1, 1];
        } else if self.last == [0, mag] {
            self.delta = [-1, -1];
        } else if self.last == [-mag, 0] {
            self.delta = [1, -1];
        } else if self.last == [0, -mag] {
            self.delta = [1, 1];
        };

        let [x, y] = self.last;
        let [dx, dy] = self.delta;
        self.last = [x + dx, y + dy];
        if self.last == [mag, 0] {
            self.last = [mag + 1, 0];
            self.mag += 1;
        }
        Some(self.last)
    }
}
impl Hailstone {
    fn shift_vel2(self, [dvx, dvy]: [isize; 2]) -> Hailstone {
        let Self { pos, vel: [vx, vy, vz] } = self;
        Self { pos, vel: [vx - dvx, vy - dvy, vz] }
    }
}