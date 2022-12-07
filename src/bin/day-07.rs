use std::borrow::Borrow;
use std::collections::{BTreeMap, BTreeSet, VecDeque};
use std::fs;
use std::path::{PathBuf, Path};

fn main() {
    let input = fs::read_to_string("inputs/7.txt").unwrap();

    let mut sh = Shell::new();

    let mut dq: VecDeque<_> = input.lines().collect();
    while !dq.is_empty() {
        sh.next_cmd(&mut dq);
    }

    let sum: usize = sh.dir.iter()
        .map(|d| sh.dir_size(d))
        .filter(|&size| size <= 100000)
        .sum();
    println!("{sum}");

    let unused = 8381165;
    let size = sh.dir.iter()
        .map(|d| sh.dir_size(d))
        .filter(|size| size >= &unused)
        .min()
        .unwrap();

    println!("{size}");
}

#[derive(Debug)]
struct Shell {
    pwd: PathBuf,
    dir: BTreeSet<PathBuf>,
    tree: BTreeMap<PathBuf, usize>
}
impl Shell {
    fn new() -> Self {
        Self {
            pwd: PathBuf::new(),
            dir: BTreeSet::new(),
            tree: BTreeMap::new()
        }
    }

    fn dir_size(&self, dir: &PathBuf) -> usize
    {
        self.tree.range::<PathBuf, _>(dir..)
            .filter(|(path, _)| path.starts_with(dir))
            .map(|(_, size)| size)
            .sum()
    }

    fn next_cmd(&mut self, lines: &mut VecDeque<&str>) {
        if let Some(line) = lines.pop_front() {
            if let Some(cmd) = line.strip_prefix("$ ") {
                if let Some(arg) = cmd.strip_prefix("cd ") {
                    self.cd(arg);
                } else if cmd == "ls" {
                    while lines.get(0).filter(|s| !s.starts_with('$')).is_some() {
                        self.ls(lines.pop_front().unwrap());
                    }
                }
            }
        }
    }

    fn cd(&mut self, s: &str) {
        if s == ".." {
            self.pwd.pop();
        } else {
            self.pwd.push(s);
            self.dir.insert(self.pwd.clone());
        }
    }

    fn ls(&mut self, s: &str) {
        if !s.starts_with("dir ") {
            let mut sp = s.split(' ');
            let size: usize = sp.next().unwrap().parse().unwrap();
            let file = sp.next().unwrap();

            let mut path = self.pwd.clone();
            path.push(file);
            self.tree.insert(path, size);
        }
    }
}
