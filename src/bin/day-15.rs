use std::fs;
use std::str::FromStr;

fn main() {
    let input = fs::read_to_string("inputs/15.txt").unwrap();

    let sensors: Vec<Sensor> = input.lines()
        .map(|s| s.parse().unwrap())
        .collect();
    
    let y = 2000000;
    let visible_sensors: Vec<_> = sensors.iter()
        .filter(|s| s.covers_y(y))
        .collect();
    let (xmin, xmax) = find_x_bounds(visible_sensors.iter().copied(), y);
    println!("checking {xmin}..={xmax} on {} sensors", visible_sensors.len());

    let result = (xmin..=xmax)
        .filter(|&x| visible_sensors.iter().any(|s| {
            s.covers_pos((x, y)) && (x, y) != s.nearest
        }))
        .count();
    println!("{result}");

    let range = 4000000;
    let mut result = None;
    'a: for y in 0..=range {
        // println!("checking {y}");
        let vis: Vec<_> = sensors.iter().filter(|s| s.covers_y(y)).collect();
        
        let mut xs = 0..=range;
        while let Some(x) = xs.next() {
            let m_in_range = vis.iter()
                .flat_map(|s| s.cover_range((x, y)))
                .next();
            if let Some(ir) = m_in_range {
                xs = (x + ir + 1) ..= range;
            } else {
                result.replace((x, y));
                break 'a;
            }
        }
    }
    println!("{result:?}");
    // println!("done");
}

type Coord = (isize, isize);

#[derive(Debug)]
struct Sensor {
    sensor: Coord,
    nearest: Coord,
    range: isize
}
impl Sensor {
    fn new(sensor: Coord, nearest: Coord) -> Self {
        Self { sensor, nearest, range: manhattan(sensor, nearest) }
    }

    fn covers_y(&self, y: isize) -> bool {
        (self.sensor.1 - y).abs() <= self.range
    }

    fn covers_pos(&self, p: Coord) -> bool {
        self.cover_range(p).is_some()
    }

    fn cover_range(&self, p: Coord) -> Option<isize> {
        let delta = self.range - manhattan(self.sensor, p);
        (delta >= 0).then_some(delta)
    }
}

impl FromStr for Sensor {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let c = s.strip_prefix("Sensor at x=").ok_or(())?;
        let (sx, rest) = c.split_once(", y=").unwrap();
        let (sy, rest) = rest.split_once(": closest beacon is at x=").unwrap();
        let (nx, ny) = rest.split_once(", y=").unwrap();

        Ok(Sensor::new(
            (sx.parse().unwrap(), sy.parse().unwrap()),
            (nx.parse().unwrap(), ny.parse().unwrap())
        ))
    }
}

fn manhattan((ax, ay): Coord, (bx, by): Coord) -> isize {
    (ax - bx).abs() + (ay - by).abs()
}

fn find_x_bounds<'a>(it: impl IntoIterator<Item=&'a Sensor>, y: isize) -> (isize, isize) {
    let (a, b) = it.into_iter()
        .fold((None, None), |(mut min, mut max), sensor| {
            let (sx, sy) = sensor.sensor;
            let dy = (y - sy).abs();

            if sensor.range < dy { return (min, max); }

            let pmin = sx - (sensor.range - dy);
            let pmax = sx + (sensor.range - dy);

            min = match min.as_ref() {
                Some(&v) if v > pmin => Some(pmin),
                None => Some(pmin),
                Some(_) => min
            };
            max = match max.as_ref() {
                Some(&v) if v < pmax => Some(pmax),
                None => Some(pmax),
                Some(_) => max
            };

            (min, max)
        });

    a.zip(b).unwrap()
}