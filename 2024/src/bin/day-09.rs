fn main() {
    let input = std::fs::read_to_string("inputs/09.txt").unwrap();
    soln(&input);
}

fn soln(input: &str) {
    let mut data: Vec<Option<usize>> = Vec::new();
    for (i, chunk) in input.as_bytes().chunks(2).enumerate() {
        let n = usize::from(chunk[0] - b'0');
        data.extend(std::iter::repeat_n(Some(i), n));

        if let Some(&bm) = chunk.get(1) {
            let m = usize::from(bm - b'0');
            data.extend(std::iter::repeat_n(None, m));
        }
    }

    let mut i = 0;
    let mut j = data.len() - 1;

    while i < j {
        if data[i].is_some() {
            i += 1;
            continue;
        }
        if data[j].is_none() {
            j -= 1;
            continue;
        }
        data.swap(i, j);
        i += 1;
        j -= 1;
    }

    let p1 = data.into_iter()
        .take_while(|s| s.is_some())
        .flatten()
        .enumerate()
        .map(|(i, n)| i * n)
        .sum::<usize>();
        
    println!("{p1}");

    let mut data: Vec<(usize, Option<usize>)> = Vec::new();
    for (i, chunk) in input.as_bytes().chunks(2).enumerate() {
        let n = usize::from(chunk[0] - b'0');
        data.push((n, Some(i)));

        if let Some(&bm) = chunk.get(1) {
            let m = usize::from(bm - b'0');
            data.push((m, None));
        }
    }

    let mut i = 0;

    while i < data.len() {
        let (di_size, di_block) = data[i];
        if di_block.is_some() {
            i += 1;
            continue;
        }

        if let Some(j) = data[i..].iter().rposition(|&(dj_size, dj_block)| dj_block.is_some() && dj_size <= di_size) {
            let (dj_size, _) = data[i + j];
            data.splice(i..=i, [(dj_size, di_block), (di_size - dj_size, di_block)]);
            data.swap(i, i + j + 1);
        }

        i += 1;
    }

    let p2 = data.into_iter()
        .flat_map(|(size, block)| std::iter::repeat_n(block, size))
        .enumerate()
        .filter_map(|(i, ms)| ms.map(|s| s * i))
        .sum::<usize>();
        
    println!("{p2}");

}