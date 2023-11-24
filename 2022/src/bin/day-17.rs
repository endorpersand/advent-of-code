use std::collections::{VecDeque, HashMap, BTreeMap};
use std::fs;

fn main() {
    let input = fs::read_to_string("inputs/17.txt").unwrap();
    let mut instructions = Instructions::new(&input);

    let overlays: [Vec<u8>; 5] = [
        vec![
            0b0011110,
        ],
        vec![
            0b0001000,
            0b0011100,
            0b0001000,
        ],
        vec![
            0b0000100,
            0b0000100,
            0b0011100,
        ],
        vec![
            0b0010000,
            0b0010000,
            0b0010000,
            0b0010000,
        ],
        vec![
            0b0011000,
            0b0011000,
        ]
    ];

    // part A
    let oiter = overlays.iter().cloned().cycle();

    let mut field = Field::new();
    for rock in oiter.take(2022) {
        field.add_overlay(&rock);
        instructions.apply(&mut field);
    }
    println!("{}", field.field.len());

    // part B
    let oiter = overlays.iter().cloned().enumerate().cycle();
    let mut field = Field::new();
    instructions.1 = 0;

    let mut map: HashMap<_, Vec<_>> = HashMap::new();
    for (j, (t, rock)) in oiter.enumerate() {
        let ent = map.entry((instructions.1, t)).or_default();
        if ent.len() >= 2 {
            break;
        } else {
            ent.push((j /* index of rock */, field.field.len() /* height of field */));
        }

        field.add_overlay(&rock);
        instructions.apply(&mut field);
    }

    // println!("{map:?}");

    let periodics: BTreeMap<_, _> = map.into_values()
        .filter_map(|v| <[_; 2]>::try_from(v).ok())
        .map(|[(start, sh), (end, eh)]| (start, (end - start, eh - sh, sh)))
        .collect();

    let p1 = periodics.iter();
    let mut p2 = periodics.iter();
    p2.next();
    let (&start, &(m, dy, _)) = std::iter::zip(p1, p2)
        .filter_map(|(a @ (k1, _), (&k2, _))| (k1 + 1 == k2).then_some(a))
        .next()
        .unwrap();

    let x = 1000000000000 - start;
    let (cuts, rest) = (x / m, x % m);

    let (_, _, mh) = periodics[&(start + rest)];

    let result = cuts * dy + mh;
    println!("{result}");
}

// ####

// .#.
// ###
// .#.

// ..#
// ..#
// ###

// #
// #
// #
// #

// ##
// ##

struct Instructions(Vec<bool>, usize);
impl Instructions {
    fn new(input: &str) -> Self {
        Self(input.chars().map(|c| c == '<').collect(), 0)
    }

    fn apply(&mut self, field: &mut Field) -> usize {
        let mut ctr = 0;
        
        loop {
            let left = self.0[self.1];
            self.1 = (self.1 + 1) % self.0.len();
            ctr += 1;

            if left {
                field.shift_left();
            } else {
                field.shift_right();
            }
            if !field.shift_down() { break ctr; }
        }
    }
}

struct Field {
    field: VecDeque<u8>, // left side is top
    overlay: Option<(Vec<u8>, usize)>,
}

impl Field {
    fn new() -> Self {
        Self {
            field: VecDeque::new(),
            overlay: None,
        }
    }

    fn add_overlay(&mut self, overlay: &[u8]) {
        // add 3 spaces + overlay space
        for _ in 0..(3 + overlay.len()) {
            self.field.push_front(0);
        }
        
        self.overlay = Some((overlay.to_owned(), 0))
    }

    fn merge_overlay(&mut self) {
        if let Some((overlay, shift)) = self.overlay.take() {
            let field = align_field_mut(self.field.make_contiguous(), shift);
            for (s, o) in std::iter::zip(field, overlay) {
                *s |= o;
            }

            while let Some(&back) = self.field.front() {
                if back != 0 { break; }
                self.field.pop_front();
            }
        }
    }

    fn shift_left(&mut self) -> bool {
        if let Some((overlay, shift)) = self.overlay.as_mut() {
            // there's space to shift
            let has_space = overlay.iter().all(|r| r & 0b1000000 == 0);
            let no_collide = collision_check(self.field.make_contiguous(), overlay, *shift, 
                |f, o| f & (o << 1) == 0
            );

            if has_space && no_collide {
                for r in overlay.iter_mut() {
                    *r <<= 1;
                }
            }

            has_space && no_collide
        } else {
            false
        }
    }
    fn shift_right(&mut self) -> bool {
        if let Some((overlay, shift)) = self.overlay.as_mut() {
            // there's space to shift
            let has_space = overlay.iter().all(|r| r & 0b0000001 == 0);
            let no_collide = collision_check(self.field.make_contiguous(), overlay, *shift, 
                |f, o| f & (o >> 1) == 0
            );

            if has_space && no_collide {
                for r in overlay.iter_mut() {
                    *r >>= 1;
                }
            }

            has_space && no_collide
        } else {
            false
        }
    }

    // print if continue
    fn shift_down(&mut self) -> bool {
        if let Some((overlay, shift)) = self.overlay.as_mut() {
            let no_collide = collision_check(self.field.make_contiguous(), overlay, *shift + 1, 
                |f, o| f & o == 0
            );

            if no_collide {
                *shift += 1;
            } else {
                self.merge_overlay();
            }

            no_collide
        } else {
            false
        }
    }
}

fn align_field(field: &[u8], shift: usize) -> &[u8] {
    let (_, start) = field.split_at(shift.min(field.len()));
    start
}
fn align_field_mut(field: &mut [u8], shift: usize) -> &mut [u8] {
    let (_, start) = field.split_at_mut(shift.min(field.len()));
    start
}

fn collision_check(field: &[u8], overlay: &[u8], shift: usize, f: impl Fn(u8, u8) -> bool) -> bool {
    let field = align_field(field, shift);

    let fieldit = field.iter().chain(std::iter::repeat(&0b1111111));
    std::iter::zip(fieldit, overlay.iter())
        .all(|(&a, &b)| f(a, b))
}

impl std::fmt::Display for Field {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for row in self.field.iter() {
            writeln!(f, "{:07b}", row)?
        }

        Ok(())
    }
}