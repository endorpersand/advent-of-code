use std::fmt::Debug;
use std::fs;
use std::str::FromStr;

fn main() {
    let input = fs::read_to_string("inputs/13.txt").unwrap();

    let mut cmps: Vec<(List<usize>, List<usize>)> = vec![];
    let mut lines = input.lines();
    while let Some((a, b)) = lines.next().zip(lines.next()) {
        cmps.push((a.parse().unwrap(), b.parse().unwrap()));
        lines.next();
    }

    let order: usize = cmps.iter().enumerate()
        .filter_map(|(i, (a, b))| (a < b).then_some(i + 1))
        .sum();
    println!("{order:?}");

    let mut all: Vec<List<usize>> = cmps
        .into_iter()
        .flat_map(|(a, b)| [a, b])
        .collect();
    
    let (k1, k2): (List<_>, List<_>) = (
        "[[2]]".parse().unwrap(),
        "[[6]]".parse().unwrap()
    );

    all.push(k1.clone());
    all.push(k2.clone());
    all.sort();
    let mut allit = all.iter().enumerate();
    let x = allit.by_ref().find_map(|(i, lst)| (lst == &k1).then_some(i + 1)).unwrap();
    let y = allit.by_ref().find_map(|(i, lst)| (lst == &k2).then_some(i + 1)).unwrap();
    println!("{}", x * y);

}

#[derive(PartialEq, Eq, Clone)]
enum List<T> {
    Element(T),
    Vec(Vec<List<T>>)
}

fn split_numeric(s: &str) -> Option<(usize, &str)> {
    let first_non_numeric = s.find(|c: char| !c.is_numeric())?;
    let (a, b) = s.split_at(first_non_numeric);
    Some((a.parse().unwrap(), b))
}

fn split_list(s: &str) -> Option<(List<usize>, &str)> {
    let mut ss = s.strip_prefix('[')?;
    let mut inner = vec![];
    loop {
        let rest = match ss.chars().next()? {
            c if c.is_numeric() => {
                let (n, rest) = split_numeric(ss)?;
                inner.push(List::Element(n));
                rest
            },
            '[' => {
                let (lst, rest) = split_list(ss)?;
                inner.push(lst);
                rest
            },
            _ => break
        };
        if let Some(rest) = rest.strip_prefix(',') {
            ss = rest;
        } else {
            ss = rest;
            break;
        }
    }
    let ss = ss.strip_prefix(']')?;
    
    Some((List::Vec(inner), ss))
}

impl FromStr for List<usize> {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (a, b) = split_list(s).ok_or(())?;
        b.is_empty().then_some(a).ok_or(())
    }
}

impl<T: Debug> std::fmt::Debug for List<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Element(arg0) => write!(f, "{arg0:?}"),
            Self::Vec(arg0) => f.debug_list().entries(arg0.iter()).finish(),
        }
    }
}

impl<T: Ord> PartialOrd for List<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl<T: Ord> Ord for List<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (self, other) {
            (List::Element(e1), List::Element(e2)) => e1.cmp(e2),
            (List::Vec(v1), List::Vec(v2)) => v1.cmp(v2),
            (List::Element(_), List::Vec(v2)) => std::iter::once(self).cmp(v2),
            (List::Vec(v1), List::Element(_)) => v1.iter().cmp(std::iter::once(other)),
        }
    }
}