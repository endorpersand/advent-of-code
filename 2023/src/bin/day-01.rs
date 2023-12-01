use std::fs;

fn main() {
    let txt = fs::read_to_string("inputs/01.txt").unwrap();
    let sum: usize = txt.lines()
        .map(get_calibration)
        .sum();
    println!("{sum}");

    // println!("{}", get_calibration_b(txt.lines().next().unwrap()));
    let sum: usize = txt.lines()
        .map(get_calibration_b)
        .sum();
    println!("{sum}");

    let sum: usize = txt.lines()
        .map(get_calibration_b_refactored)
        .sum();
    println!("{sum}");
}

fn get_calibration(s: &str) -> usize {
    let mut chars = s.chars()
        .filter(|c| c.is_ascii_digit());

    let beginning = chars.next().unwrap();
    let end = chars.next_back().unwrap_or(beginning);
    String::from_iter([beginning, end])
        .parse()
        .unwrap()
}

fn get_calibration_b(s: &str) -> usize {
    let counts = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"];
    let mut chars = s.chars()
        .enumerate()
        .filter(|(_, c)| c.is_ascii_digit())
        .map(|(i, c)| (String::from(c).parse::<usize>().unwrap(), i));

    let left_txt = counts.into_iter()
        .enumerate()
        .filter_map(|(i, mtch)| s.find(mtch).map(|j| (i, j)))
        .min_by_key(|(_, mi)| *mi);
    let left_n = chars.next();
    let left = match (left_txt, left_n) {
        (Some((tv, ti)), Some((nv, ni))) => if ti < ni { tv } else { nv },
        (Some((tv, _)), _)  => tv,
        (_, Some((nv, _))) => nv,
        _ => unreachable!()
    };

    let right_txt = counts.into_iter()
        .enumerate()
        .filter_map(|(i, mtch)| s.rfind(mtch).map(|j| (i, j)))
        .max_by_key(|(_, mi)| *mi);
    let right_n = chars.last().or(left_n);
    let right = match (right_txt, right_n) {
        (Some((tv, ti)), Some((nv, ni))) => if ti > ni { tv } else { nv },
        (Some((tv, _)), _)  => tv,
        (_, Some((nv, _))) => nv,
        _ => unreachable!()
    };

    left * 10 + right
}
fn get_calibration_b_refactored(s: &str) -> usize {
    let counts = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"];

    let left = counts.into_iter()
        .enumerate()
        .filter_map(|(val, key)| s.find(key).map(|idx| (idx, val)))
        .chain(s.find(|c: char| c.is_ascii_digit()).map(|idx| (idx, s[idx..(idx+1)].parse().unwrap())))
        .min()
        .unwrap()
        .1;
    let right = counts.into_iter()
        .enumerate()
        .filter_map(|(val, key)| s.rfind(key).map(|idx| (idx, val)))
        .chain(s.rfind(|c: char| c.is_ascii_digit()).map(|idx| (idx, s[idx..(idx+1)].parse().unwrap())))
        .max()
        .map_or(left, |(_, val)| val);
    
    left * 10 + right
}