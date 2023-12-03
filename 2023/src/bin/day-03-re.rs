// For original, see day-03.rs

use std::collections::HashMap;
use std::ops::{Range, RangeInclusive};

use once_cell::sync::Lazy;
use regex::Regex;

fn main() {
    let txt = std::fs::read_to_string("inputs/03.txt").unwrap();

    let ranges = get_lit_ranges(&txt);
    let mut symbols = find_symbols(&txt);

    let out: usize = ranges.iter()
        .filter(|Numeric { bounds, value: _ }| {
            symbols.keys()
                .any(|&center| Bounds::new_from_polar(center, 1).overlaps(bounds))
            })
        .map(|Numeric { bounds: _, value }| value)
        .sum();
    println!("{out}");

    symbols.retain(|_, &mut c| c == '*');
    let mut ctr: HashMap<_, Vec<_>> = HashMap::new();
    
    for Numeric { bounds, value } in ranges.iter() {
        let Some(&hit) = symbols.keys()
            .find(|&&center| Bounds::new_from_polar(center, 1).overlaps(bounds)) 
            else { continue };

        ctr.entry(hit).or_default().push(value);
    }
    let out: usize = ctr.iter()
        .filter_map(|(_, values)| match &**values {
            &[a, b] => Some(a * b),
            _ => None
        })
        .sum();
    println!("{out}");
}

struct Bounds {
    rx: RangeInclusive<usize>,
    ry: RangeInclusive<usize>,
}
impl Bounds {
    fn new(rx: RangeInclusive<usize>, ry: RangeInclusive<usize>) -> Option<Bounds> {
        (rx.start() <= rx.end() && ry.start() <= ry.end()).then_some(Bounds { rx, ry })
    }
    fn new_from_stream(r: Range<usize>, line_len: usize) -> Option<Bounds> {
        let (sx, sy) = stream_coord(r.start, line_len);
        let (ex, ey) = stream_coord(r.end - 1, line_len); // insert inclusion
        
        Bounds::new(sx..=ex, sy..=ey)
    }
    fn new_from_polar(center: (usize, usize), rad: usize) -> Bounds {
        let (cx, cy) = center;
        
        Bounds::new(
            cx.saturating_sub(rad) ..= cx.saturating_add(rad),
            cy.saturating_sub(rad) ..= cy.saturating_add(rad),
        ).unwrap()
    }

    fn overlaps(&self, other: &Bounds) -> bool {
        let &start_x = self.rx.start().max(other.rx.start());
        let &start_y = self.ry.start().max(other.ry.start());
        let &end_x = self.rx.end().min(other.rx.end());
        let &end_y = self.ry.end().min(other.ry.end());

        Bounds::new(start_x..=end_x, start_y..=end_y).is_some()
    }
}

fn stream_coord(i: usize, line_len: usize) -> (usize, usize) {
    (i % (line_len + 1), i / (line_len + 1))
}
fn find_symbols(file: &str) -> HashMap<(usize, usize), char> {
    file.lines()
        .enumerate()
        .flat_map(|(lno, line)| {
            line.char_indices()
                .filter(|&(_, c)| c.is_ascii_punctuation() && c != '.')
                .map(move |(cno, c)| ((cno, lno), c))
        })
        .collect()
}
struct Numeric {
    bounds: Bounds,
    value: usize
}
fn get_lit_ranges(file: &str) -> Vec<Numeric> {
    static RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"\d+").unwrap());

    let line_len = file.find('\n').unwrap();
    RE.find_iter(file)
        .map(|m| Numeric {
            bounds: Bounds::new_from_stream(m.range(), line_len).unwrap(),
            value: m.as_str().parse().unwrap()
        })
        .collect()
}