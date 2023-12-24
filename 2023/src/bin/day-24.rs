use std::num::{NonZeroU128, NonZeroI128};

const MIN: i128 = 200_000_000_000_000;
const MAX: i128 = 400_000_000_000_000;
// const MIN: i128 = 7;
// const MAX: i128 = 27;

fn main() {
    println!("{}", Rational::from(19));
    println!("{}", Rational::new(-14, NonZeroI128::new(3).unwrap()));
    println!("{}", Rational::from(19) + Rational::new(-14, NonZeroI128::new(3).unwrap()));

    let txt = std::fs::read_to_string("inputs/24.txt").unwrap();
    let State { stones } = parse(&txt);

    // PART A
    let intersects = (0..stones.len())
        .flat_map(|i| ((i + 1)..stones.len()).map(move |j| (i, j)))
        .filter_map(|(i, j)| stones[i].intersect2d(stones[j]))
        .filter(|&([t0, t1], [x, y])| {
            t0 >= 0 && t1 >= 0 && (MIN..=MAX).contains(&x) && (MIN..=MAX).contains(&y)
        })
        .count();
    println!("{intersects}");

    // PART B
    // Take 3 hailstones :D
    let [s0, s1, s2] = stones[..3] else { unreachable!() };

    // We are trying to find r, v in L(t): r + vt

    // For every hailstone i, with pos ri and vel vi,
    // we know there exists some ti such that 
    // ri + vi ti = r + v ti.

    // Rearranging, this is ri + (vi - v) ti = r.
    // Thus, we iterate through all the possible v's for our rock.
    // If these 3 rocks collide at the same place,
    // that is `r`. The time they collide is `ti` (for each rock).
    
    // We created a 2D intersect function in part A, so we'll use that
    // and infer Z.
    
    // Iterate through possible 2D velocities for the rock
    for rv2 in Lattice2Iter::new() {
        // Shift the velocities of the hailstone by the potential vel of the rock.
        let s0s = s0.shift_vel2(rv2);
        let s1s = s1.shift_vel2(rv2);
        let s2s = s2.shift_vel2(rv2);

        // Find the intersction point
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
            let z0 = Rational::from(s0.pos[2] as i128) + t0 * Rational::from(s0.vel[2] as i128);
            let z1 = Rational::from(s1.pos[2] as i128) + t1 * Rational::from(s1.vel[2] as i128);

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

/***** RATIONAL IMPL *****/
// Using 128 bits, since the input data is ~49 bits,
// and multiplying those exceed a u64
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct Rational {
    sign: bool,
    numer: u128,
    denom: NonZeroU128
}

fn gcd(mut a: u128, mut b: u128) -> u128 {
    while b != 0 {
        [a, b] = [b, a % b];
    }
    a
}
fn lcm(a: u128, b: u128) -> u128 {
    a * b / gcd(a, b)
}

impl Rational {
    const ZERO: Self = Rational { sign: false, numer: 0, denom: unsafe { NonZeroU128::new_unchecked(1) }};

    fn new(n: i128, d: NonZeroI128) -> Self {
        let sign = n.is_negative() ^ d.is_negative();
        let numer = n.unsigned_abs();
        let denom = d.unsigned_abs();

        Self::simplify(Self { sign, numer, denom })
    }

    fn simplify(self) -> Self {
        let Self { sign, mut numer, mut denom } = self;

        if numer == 0 { return Self::ZERO };

        let gcd = gcd(numer, denom.get());
        numer /= gcd;
        denom = NonZeroU128::new(denom.get() / gcd)
            .expect("gcd should have been non-zero");

        Self { sign, numer, denom }
    }
}
impl From<i128> for Rational {
    fn from(value: i128) -> Self {
        Self {
            sign: value.is_negative(), 
            numer: value.unsigned_abs(), 
            denom: unsafe { NonZeroU128::new_unchecked(1) }
        }
    }
}
impl std::ops::Add for Rational {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        let nd = NonZeroU128::new(lcm(self.denom.get(), rhs.denom.get()))
            .expect("lcm should have been non-zero");

        let lfactor = nd.get() / self.denom;
        let rfactor = nd.get() / rhs.denom;

        let lnumer = self.numer * lfactor; 
        let rnumer = rhs.numer * rfactor;
        let (ns, nn) = if self.sign == rhs.sign {
            (self.sign, lnumer + rnumer)
        } else {
            // find absolute difference
            // base sign on the larger mag
            if lnumer >= rnumer {
                (self.sign, lnumer - rnumer)
            } else {
                (rhs.sign, rnumer - lnumer)
            }
        };

        Self::simplify(Self { sign: ns, numer: nn, denom: nd })
    }
}
impl std::ops::Neg for Rational {
    type Output = Self;

    fn neg(mut self) -> Self::Output {
        if self == Rational::ZERO { return self; }

        self.sign = !self.sign;
        self
    }

}
impl std::ops::Sub for Rational {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        self + (-rhs)
    }
}
impl std::ops::Mul for Rational {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        let Self { sign: lsign, numer: lnumer, denom: ldenom } = self;
        let Self { sign: rsign, numer: rnumer, denom: rdenom } = rhs;

        Self::simplify(Self {
            sign: lsign ^ rsign,
            numer: lnumer * rnumer,
            denom: ldenom.checked_mul(rdenom).expect("unexpected overflow")
        })
    }
}
impl std::ops::Div for Rational {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        assert_ne!(rhs, Rational::ZERO, "division by zero");

        let Self { sign, numer, denom } = rhs;
        let rhsi = Self {
            sign, 
            numer: denom.get(), 
            denom: NonZeroU128::new(numer).expect("expected numer non-zero")
        };

        self * rhsi
    }

}
impl PartialOrd for Rational {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for Rational {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let &Self { sign: lsign, numer: lnumer, denom: ldenom } = self;
        let &Self { sign: rsign, numer: rnumer, denom: rdenom } = other;

        lsign.cmp(&rsign).reverse()
            .then_with(|| {
                let mag_cmp = (lnumer * rdenom.get()).cmp(&(rnumer * ldenom.get()));
                if lsign { mag_cmp.reverse() } else { mag_cmp }
            })
    }
}
impl PartialEq<i128> for Rational {
    fn eq(&self, &other: &i128) -> bool {
        self.eq(&Rational::from(other))
    }
}
impl PartialOrd<i128> for Rational {
    fn partial_cmp(&self, &other: &i128) -> Option<std::cmp::Ordering> {
        self.partial_cmp(&Rational::from(other))
    }
}
impl PartialEq<Rational> for i128 {
    fn eq(&self, other: &Rational) -> bool {
        Rational::from(*self).eq(other)
    }
}
impl PartialOrd<Rational> for i128 {
    fn partial_cmp(&self, other: &Rational) -> Option<std::cmp::Ordering> {
        Rational::from(*self).partial_cmp(other)
    }
}

impl std::fmt::Display for Rational {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, 
            "{}{}/{}",
            if self.sign { '-' } else { '+' },
            self.numer,
            self.denom
        )
    }
}

/**********/

#[derive(Debug, Clone, Copy)]
struct Hailstone {
    pos: [isize; 3],
    vel: [isize; 3]
}
impl Hailstone {
    fn intersect2d(self, other: Hailstone) -> Option<([Rational; 2] /* time */, [Rational; 2] /* pos */)> {
        let Hailstone { pos: [x0, y0, _], vel: [vx0, vy0, _] } = self;
        let Hailstone { pos: [x1, y1, _], vel: [vx1, vy1, _] } = other;

        let [x0, y0, vx0, vy0, x1, y1, vx1, vy1] = [x0 as i128, y0 as i128, vx0 as i128, vy0 as i128, x1 as i128, y1 as i128, vx1 as i128, vy1 as i128];

        let d = NonZeroI128::new(vx0 * vy1 - vy0 * vx1)?;
        let t = Rational::new((y0 - y1) * vx1 - (x0 - x1) * vy1, d);
        let u = Rational::new((y0 - y1) * vx0 - (x0 - x1) * vy0, d);
        let x = Rational::from(x0) + Rational::from(vx0) * t;
        let y = Rational::from(y0) + Rational::from(vy0) * t;

        Some(([t, u], [x, y]))
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