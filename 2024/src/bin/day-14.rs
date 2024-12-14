fn main() {
    let input = std::fs::read_to_string("inputs/14.txt").unwrap();
    part1(&input);
    part2(&input);
}

type Position = (usize, usize);
type PosDelta = (isize, isize);

const WIDTH: usize = 101;
const HEIGHT: usize = 103;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Robot {
    position: Position,
    velocity: PosDelta
}
impl Robot {
    fn iterate(&mut self, time: isize) {
        self.position.0 = ((self.position.0 as isize) + (self.velocity.0 * time)).rem_euclid(WIDTH as isize) as usize;
        self.position.1 = ((self.position.1 as isize) + (self.velocity.1 * time)).rem_euclid(HEIGHT as isize) as usize;
    }
}
fn parse(input: &str) -> Vec<Robot> {
    input.lines().filter_map(|s| {
        let (pstr, vstr) = s.strip_prefix("p=")?.split_once(" v=")?;
        let (prstr, pcstr) = pstr.split_once(",")?;
        let (vrstr, vcstr) = vstr.split_once(",")?;

        Some(Robot {
            position: prstr.parse().ok().zip(pcstr.parse().ok())?,
            velocity: vrstr.parse().ok().zip(vcstr.parse().ok())?,
        })
    }).collect()
}
#[allow(dead_code)]
fn part1(input: &str) {
    use std::cmp::Ordering;

    let mut robots = parse(input);
    robots.iter_mut().for_each(|r| r.iterate(100));
    
    let mut quadrants = [0; 4];
    for r in &robots {
        match (r.position.0.cmp(&(WIDTH / 2)), r.position.1.cmp(&(HEIGHT / 2))) {
            (Ordering::Less, Ordering::Less) => quadrants[0] += 1,
            (Ordering::Less, Ordering::Greater) => quadrants[1] += 1,
            (Ordering::Greater, Ordering::Less) => quadrants[2] += 1,
            (Ordering::Greater, Ordering::Greater) => quadrants[3] += 1,
            _ => {}
        }
    }

    let p1: usize = quadrants.into_iter().product();
    println!("{p1}");
}

#[allow(dead_code)]
fn part2(input: &str) {
    let mut robots = parse(input);
    let mut iters = 0;
    let mut render = vec![[false; WIDTH]; HEIGHT];

    loop {
        // Update render:
        render.as_flattened_mut().fill(false);
        robots.iter()
            .map(|r| r.position)
            .for_each(|(x, y)| render[y][x] = true);

        // Christmas trees have a horizontal of >10
        let horiz = render.as_flattened()
            .split(|c| !c)
            .map(|chunk| chunk.len())
            .max()
            .unwrap_or(0);
        if horiz > 10 {
            // print tree
            for row in &render {
                for &cell in row {
                    print!("{}", if cell { '#' } else { '.' });
                }
                println!();
            }
            //

            break;
        }
        
        robots.iter_mut().for_each(|r| r.iterate(1));
        iters += 1;
    }

    println!("{iters}");
}