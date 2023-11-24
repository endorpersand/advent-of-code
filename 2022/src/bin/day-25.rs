use std::fs;

fn main() {
    let input = fs::read_to_string("inputs/25.txt").unwrap();

    let mut sum = 0;
    for line in input.lines() {
        let val = from_snafu(line);
        println!("{line} => {val}");
        sum += val;
    }
    println!("sum: {sum}");
    println!("sum-snafu: {}", to_snafu(sum));
}

fn from_snafu(s: &str) -> isize {
    let mut result = 0;
    for c in s.chars() {
        result *= 5;
        result += match c {
            '2' => 2,
            '1' => 1,
            '0' => 0,
            '-' => -1,
            '=' => -2,
            _ => unimplemented!()
        }
    }

    result
}

fn bal_mod_5(val: isize) -> isize {
    let v5 = val.rem_euclid(5);
    if v5 > 2 { v5 - 5 } else { v5 }
}

fn to_snafu(mut val: isize) -> String {
    let mut vec = vec![];

    while val != 0 {
        let m = bal_mod_5(val);
        let c = match m {
            2 => '2',
            1 => '1',
            0 => '0',
            -1 => '-',
            -2 => '=',
            _ => unimplemented!()
        };
        vec.push(c);

        val -= m;
        val /= 5;
    }

    vec.into_iter().rev().collect()
}