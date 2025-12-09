fn main() {
    let input = std::fs::read_to_string("inputs/09.txt").unwrap();
    println!("{}", part1(&input));
    println!("{}", part2(&input));
}

type Point = [usize; 2];
type LineSeg = [Point; 2];

fn area(v: Point, w: Point) -> usize {
    let [vx, vy] = v;
    let [wx, wy] = w;
    (vx.abs_diff(wx) + 1) * (vy.abs_diff(wy) + 1)
}

fn parse(input: &str) -> Vec<Point> {
    input.lines()
        .map(|l| {
            let mut it = l.split(',');
            std::array::from_fn(|_| it.next().unwrap().parse().unwrap())
        }).collect()
}
fn part1(input: &str) -> usize {
    let vectors = parse(input);

    vectors.iter().enumerate()
        .flat_map(|(i, &v)| {
            vectors[i+1 ..].iter()
                .map(move |&w| area(v, w))
        })
        .max()
        .unwrap()
}

fn poly_segs(poly: &[Point]) -> impl Iterator<Item=LineSeg> {
    (0..poly.len())
        .map(move |i| (i, (i + 1) % poly.len()))
        .map(|(i, j)| [poly[i], poly[j]])
}

fn point_in_poly(poly: &[Point], p: Point) -> bool {
    let [px, py] = p;

    let on_boundary = {
        poly_segs(poly)
            .any(|[[l1x, l1y], [l2x, l2y]]| {
                if l1x == l2x {
                    l1x == px && ((l1y <= py && py <= l2y) || (l2y <= py && py <= l1y))
                } else if l1y == l2y {
                    ((l1x <= px && px <= l2x) || (l2x <= px && px <= l1x)) && l1y == py
                } else {
                    unimplemented!()
                }

            })
    };
    on_boundary || {
        // check if any line intersects with [px, py] - [infty, py] (horizontal)
        poly_segs(poly)
            .filter(|&[[l1x, l1y], [l2x, l2y]]| {
                if l1x == l2x {
                    // is vertical
                    px < l1x && ((l1y < py && py < l2y) || (l2y < py && py < l1y))
                } else if l1y == l2y {
                    // is horizontal
                    py == l1y && px < l1x && px < l2x
                } else {
                    unimplemented!()
                }
            })
            .count() % 2 != 0
    }
}
fn part2(input: &str) -> usize {
    let vectors = parse(input);

    // let a = vectors.iter().enumerate()
    //     .flat_map(|(i, &v)| {
    //         // [vectors[i], vectors[(i + 2) % vectors.len()]]
    //         vectors[i+1 ..].iter()
    //             .map(move |&w| [v, w])
    //     })
    //     .filter(|&[[lx, ly], [rx, ry]]| {
    //         point_in_poly(&vectors, [lx, ry])
    //             && point_in_poly(&vectors, [rx, ly])
    //             && ly >= 50355 && ry >= 50355
    //     })
    //     .max_by_key(|&[l, r]| area(l, r))
    //     .unwrap();
    
    let r = [94654,50355];
    let a = vectors.iter()
        .map(|&v| [v, r])
        .filter(|&[[_, vy], [_, ry]]| vy >= ry)
        .filter(|&[[lx, ly], [rx, ry]]| {
            point_in_poly(&vectors, [lx, ry])
                && point_in_poly(&vectors, [rx, ly])
        })
        .max_by_key(|&[l, r]| area(l, r))
        .unwrap();
    println!("{a:?}");
    // let [[a0x, a0y], [a1x, a1y]] = [[9,5],[2,3]];
    // println!("{:?} ~~", [a0x, a1y]);
    // println!("{}", point_in_poly(&vectors, [a0x, a1y]));
    // println!("{:?} ~~", [a1x, a0y]);
    // println!("{}", point_in_poly(&vectors, [a1x, a0y]));
    // println!("{:?} ~~", [a0x - 1, a0y - 1]);
    // println!("{}", point_in_poly(&vectors, [a0x - 1, a0y - 1]));
    area(a[0], a[1])
}
