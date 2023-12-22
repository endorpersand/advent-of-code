fn main() {
    let txt = std::fs::read_to_string("inputs/22.txt").unwrap();
    let State { bricks } = parse(&txt);

    let mut grid = Grid::from_bricks(&bricks);
    grid.drop_bricks();
    println!("{:?}", grid.count_disinteg());
    println!("{:?}", grid.count_fallers());
}

#[derive(Debug)]
struct State {
    bricks: Vec<Brick>
}
fn parse(file: &str) -> State {
    let bricks = file.lines()
        .map(|line| {
            let (min, max) = line.split_once('~').unwrap();
            let bmin = min.split(',')
                .map(|s| s.parse().unwrap())
                .collect::<Box<[_]>>();
            let bmax = max.split(',')
                .map(|s| s.parse().unwrap())
                .collect::<Box<[_]>>();

            Brick {
                min: *Box::try_from(bmin).unwrap(),
                max: *Box::try_from(bmax).unwrap(),
            }
        })
        .collect();

    State { bricks }
}

#[derive(Debug, Clone, Copy)]
struct Brick {
    min: [usize; 3],
    max: [usize; 3]
}
impl Brick {
    fn intersect_projected(self, other: Brick) -> bool {
        fn intersect2d(a_min: [usize; 2], a_max: [usize; 2], b_min: [usize; 2], b_max: [usize; 2]) -> bool {
            let [ax1, ay1] = a_min;
            let [ax2, ay2] = a_max;
            let [bx1, by1] = b_min;
            let [bx2, by2] = b_max;

            (ax1 < bx2) && (ax2 > bx1) && (ay1 < by2) && (ay2 > by1)
        }
        
        let Brick { min: [a_min @ .., _], max: [ax2, ay2, _] } = self;
        let Brick { min: [b_min @ .., _], max: [bx2, by2, _] } = other;
        
        intersect2d(a_min, [ax2 + 1, ay2 + 1], b_min, [bx2 + 1, by2 + 1])
    }

    fn lower(self, n: usize) -> Self {
        let Brick { min: [x1, y1, z1], max: [x2, y2, z2] } = self;
        Brick { min: [x1, y1, z1 - n], max: [x2, y2, z2 - n] }
    }
}

#[allow(unused)]
const X: usize = 0;
#[allow(unused)]
const Y: usize = 1;
const Z: usize = 2;

#[derive(Debug, Clone)]
struct Grid {
    // each index is the z of each brick,
    // 0 = z1
    // 1 = z2
    // etc
    data: Vec<Vec<Brick>>
}
impl Grid {
    fn from_bricks(bricks: &[Brick]) -> Grid {
        let max_z = bricks.iter()
            .map(|t| t.max[Z])
            .max()
            .expect("bricks should be non-empty");

        let mut data = vec![vec![]; max_z];
        for &b in bricks {
            data[b.max[2] - 1].push(b);
        }

        Grid { data }
    }

    fn try_lower(&self, brick: Brick) -> usize {
        // go down k tiles, check if it intersects anywhere
        // if so, then it can move k - 1 tiles
        let m_delta_p1 = (1usize..).find(|k| {
            let lowered = brick.min[Z] - k;
            lowered == 0 || {
                self.data[lowered - 1]
                    .iter()
                    .any(|&other| brick.intersect_projected(other))
            }
        });
    
        m_delta_p1.unwrap() - 1
    }
    fn drop_bricks(&mut self) -> usize {
        let mut dropped = 0;

        for i in 1..self.data.len() {
            if self.data[i].is_empty() { continue; }
            let mut lowers = vec![vec![]; 4];
            let plane = std::mem::take(&mut self.data[i]);

            for brick in plane {
                let delta = self.try_lower(brick);
                if delta >= lowers.len() {
                    lowers.resize_with(delta + 1, Vec::default);
                }
                lowers[delta].push(brick.lower(delta));
            }

            dropped += lowers[1..].iter().map(|l| l.len()).sum::<usize>();
            for (n, l) in lowers.into_iter().enumerate() {
                if !l.is_empty() {
                    self.data[i - n].extend(l);
                }
            }
        }

        dropped
    }

    fn count_disinteg(&self) -> usize {
        let mut grid = self.clone();
        let mut sum = 0;

        for i in 0..grid.data.len() {
            if grid.data[i].is_empty() { continue; }
            let row = grid.data[i].clone();

            for j in 0..row.len() {
                grid.data[i].remove(j);
                let no_fall = grid.data[(i + 1)..]
                    .iter()
                    .flatten()
                    .all(|&b| grid.try_lower(b) == 0);

                sum += usize::from(no_fall);
                grid.data[i] = row.clone();
            }
        }

        sum
    }

    fn count_fallers(&self) -> usize {
        let mut grid = self.clone();
        let mut sum = 0;

        for i in 0..grid.data.len() {
            if grid.data[i].is_empty() { continue; }

            for j in 0..grid.data[i].len() {
                grid.data[i].remove(j);
                sum += grid.drop_bricks();
                grid = self.clone();
            }
        }

        sum
    }
}