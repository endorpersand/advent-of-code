use once_cell::sync::Lazy;
use regex::Regex;

fn main() {
    let txt = std::fs::read_to_string("inputs/18.txt").unwrap();
    let State { orders } = parse(&txt);

    println!("{}", find_area_a(&orders));
    println!("{}", find_area_b(&orders));
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
enum Dir {
    Right = 0, Down = 1, Left = 2, Up = 3
}
impl Dir {
    fn shift(self) -> (isize, isize) {
        match self {
            Dir::Up    => (-1,  0),
            Dir::Right => ( 0,  1),
            Dir::Down  => ( 1,  0),
            Dir::Left  => ( 0, -1),
        }
    }
}
#[derive(Debug, Clone, Copy)]
struct Order {
    dir: Dir,
    ct: usize,
    color: u32
}
struct State {
    orders: Vec<Order>
}
fn parse(file: &str) -> State {
    static RE: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"(\w) (\d+) \(#([0-9a-f]{6})\)").unwrap()
    });

    let orders = file.lines()
        .map(|h| {
            let cap = RE.captures(h).unwrap();
            
            let dir = match &cap[1] {
                "U" => Dir::Up,
                "R" => Dir::Right,
                "D" => Dir::Down,
                "L" => Dir::Left,
                _   => unreachable!()
            };
            let ct = cap[2].parse().unwrap();
            let color = u32::from_str_radix(&cap[3], 16).unwrap();
            Order {
                dir, ct, color
            }
        })
        .collect();

    State { orders }
}

fn find_area_a(orders: &[Order]) -> isize {
    let mut points = vec![(0, 0)];
    for Order { dir, ct, color: _ } in orders {
        let &(x, y) = points.last().unwrap();
        let (mut dx, mut dy) = dir.shift();
        dx *= *ct as isize;
        dy *= *ct as isize;

        points.push((x + dx, y + dy));
    }

    // do shoelaces
    let doubled_area = points.windows(2)
        .map(|s| [s[0], s[1]])
        .map(|[(r1, c1), (r2, c2)]| r1 * c2 - c1 * r2)
        .sum::<isize>()
        .abs();

    // count boundary points:
    let boundary_pts: isize = orders.iter().map(|Order { ct, .. }| *ct as isize).sum();

    // do picks to count number of lattice pts
    // A = inner + outer / 2 - 1
    // so inner = A - outer / 2 + 1
    let inner_pts = doubled_area / 2 - boundary_pts / 2 + 1;

    inner_pts + boundary_pts
}
fn find_area_b(orders: &[Order]) -> isize {
    let norders: Vec<_> = orders.iter()
        .map(|Order { color, .. }| *color)
        .map(|color| {
            let (ct, d) = ((color >> 4) as usize, color & 3);
            let dir = unsafe { std::mem::transmute::<u8, Dir>(d as u8) };
            dbg!(Order { dir, ct, color })
        })
        .collect();

    find_area_a(&norders)
}