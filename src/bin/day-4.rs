use std::fs;
use std::ops::{RangeBounds, RangeInclusive};


fn main() {
    let input = fs::read_to_string("inputs/4.txt").unwrap();

    // PART A
    let result: usize = input.lines()
         .map(|line| {
            let (left, right) = as_ranges(line);
            subrange(left.clone(), right.clone()) || subrange(right, left)
         })
         .map(|b| b as usize)
         .sum();
    
    println!("{result}");

    // PART B
    let result: usize = input.lines()
         .map(|line| {
            let (left, right) = as_ranges(line);
            overlaps(left, right)
         })
         .map(|b| b as usize)
         .sum();
    
    println!("{result}");
}


// PART A
fn subrange<T, R>(sup: R, sub: R) -> bool 
    where T: PartialOrd,
        R: RangeBounds<T> + IntoIterator<Item=T>
{
    sub.into_iter()
       .all(|t| sup.contains(&t))
}

fn as_ranges(line: &str) -> (RangeInclusive<usize>, RangeInclusive<usize>) {
    let mut split = line.split(",");
    let left  = as_range(split.next().unwrap());
    let right = as_range(split.next().unwrap());
    debug_assert_eq!(split.next(), None);

    (left, right)
}

fn as_range(range: &str) -> RangeInclusive<usize> {
    let mut split = range.split("-");
    let left  = split.next().unwrap().parse().unwrap();
    let right = split.next().unwrap().parse().unwrap();
    debug_assert_eq!(split.next(), None);

    left..=right
}

// PART B
fn overlaps(left: RangeInclusive<usize>, right: RangeInclusive<usize>) -> bool
{
    left.into_iter()
        .any(|t| right.contains(&t))
}