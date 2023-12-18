use once_cell::sync::Lazy;
use regex::Regex;

fn main() {
    let txt = std::fs::read_to_string("inputs/18.txt").unwrap();
    let State { orders } = parse(&txt);

    println!("{}", find_area_a(orders.iter().map(|o| o.0)));
    println!("{}", find_area_b(orders.iter().map(|o| o.1)));
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
    ct: isize
}
struct State {
    // order and color
    orders: Vec<(Order, u32)>
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
            (Order { dir, ct }, color)
        })
        .collect();

    State { orders }
}

fn find_area_a(orders: impl IntoIterator<Item=Order>) -> isize {
    let mut points = vec![(0, 0)];
    let mut boundary_pts = 0;

    for Order { dir, ct } in orders {
        let &(x, y) = points.last().unwrap();
        let (dx, dy) = dir.shift();

        points.push((x + dx * ct, y + dy * ct));
        boundary_pts += ct;
    }

    // do shoelaces
    let doubled_area = points.windows(2)
        .map(|s| [s[0], s[1]])
        .map(|[(r1, c1), (r2, c2)]| r1 * c2 - c1 * r2)
        .sum::<isize>()
        .abs();

    // do picks to count number of lattice pts
    // A = inner + (outer / 2) - 1
    // so inner = A - (outer / 2) + 1
    let inner_pts = doubled_area / 2 - boundary_pts / 2 + 1;

    inner_pts + boundary_pts
}
fn find_area_b(colors: impl IntoIterator<Item=u32>) -> isize {
    find_area_a({
        colors.into_iter()
            .map(|color| {
                let (ct, d) = ((color >> 4) as isize, (color & 3) as u8);
                let dir = unsafe { std::mem::transmute::<u8, Dir>(d) };

                Order { dir, ct }
            })
    })
}