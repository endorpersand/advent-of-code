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
    let mut data = parse(input);
    data.iter_mut().for_each(|r| r.iterate(100));
    
    let mut quadrants = [0; 4];
    for r in &data {
        match (r.position.0.cmp(&(WIDTH / 2)), r.position.1.cmp(&(HEIGHT / 2))) {
            (std::cmp::Ordering::Less, std::cmp::Ordering::Less) => quadrants[0] += 1,
            (std::cmp::Ordering::Less, std::cmp::Ordering::Greater) => quadrants[1] += 1,
            (std::cmp::Ordering::Greater, std::cmp::Ordering::Less) => quadrants[2] += 1,
            (std::cmp::Ordering::Greater, std::cmp::Ordering::Greater) => quadrants[3] += 1,
            _ => {}
        }
    }

    let p1: usize = quadrants.into_iter().product();
    println!("{p1}");
}

fn render(robots: &[Robot]) -> Vec<[bool; WIDTH]> {
    let mut data = vec![[false; WIDTH]; HEIGHT];
    for r in robots {
        let (x, y) = r.position;
        data[y][x] = true;
    }
    data
}
fn print_data(robots: &[Robot]) {
    let data = render(robots);
    for row in data {
        for cell in row {
            print!("{}", if cell { '#' } else { '.' });
        }
        println!();
    }
}
fn count_strip(n: &[bool]) -> usize {
    n.split(|c| !c).map(|n| n.len()).max().unwrap_or(0)
}

#[allow(dead_code)]
fn part2(input: &str) {
    let mut data = parse(input);
    let mut iters = 0;

    loop {
        let rendered = render(&data);
        if rendered.iter().any(|r| count_strip(r) > 10) {
            println!("tree {iters}");
            print_data(&data);
            break;
        }
        
        data.iter_mut().for_each(|r| r.iterate(1));
        iters += 1;
    }

    println!("{iters}");
}