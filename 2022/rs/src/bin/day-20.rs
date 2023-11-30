use std::collections::VecDeque;
use std::fs;

fn main() {
    let input = fs::read_to_string("inputs/20.txt").unwrap();
    let data: VecDeque<isize> = input.split('\n').map(|s| s.parse().unwrap()).collect();

    // part A
    let mut edata: VecDeque<_> = data.iter()
        .copied()
        .enumerate()
        .collect();
    
    mix(&mut edata);
    let mut new_data: VecDeque<_> = edata.into_iter().map(|(_, a)| a).collect();
    compute_sum(&mut new_data);

    // part B
    let mut edata: VecDeque<_> = data.iter()
        .copied()
        .map(|v| v * 811589153)
        .enumerate()
        .collect();
    for _ in 0..10 {
        mix(&mut edata);
    }
    let mut new_data: VecDeque<_> = edata.into_iter().map(|(_, a)| a).collect();
    compute_sum(&mut new_data);
}

fn shift<T: std::fmt::Debug>(vec: &mut VecDeque<(T, isize)>, i: usize) {
    // shift position to 0
    vec.rotate_left(i);
    let len = vec.len();

    let tpl @ (_, s) = vec.pop_front().unwrap();

    if s > 0 {
        vec.rotate_left(s.unsigned_abs() % (len - 1)); // after the item is removed, shift
    } else {
        vec.rotate_right(s.unsigned_abs() % (len - 1)); // after the item is removed, shift
    }
    vec.push_front(tpl);
}

fn mix(vec: &mut VecDeque<(usize, isize)>) {
    for i in 0..(vec.len()) {
        let shift_index = vec.iter().position(|&(j, _)| i == j).unwrap();
        shift(vec, shift_index);
    }
}

fn compute_sum(vec: &mut VecDeque<isize>) {
    // realign deque
    let shift_index = vec.iter().position(|&i| i == 0).unwrap();
    vec.rotate_left(shift_index);

    // take every 1k
    let mut sum = 0;
    println!("{}", vec[0]);
    for _ in 1..=3 {
        vec.rotate_left(1000 % vec.len());
        println!("{}", vec[0]);
        sum += vec[0];
    }
    println!("sum: {}", sum);
}