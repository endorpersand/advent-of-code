pub fn part1(input: &str) -> usize {
    let mut iter = input.bytes().enumerate()
        .flat_map(|(ind, d)| {
            let (i, filled) = (ind / 2, ind % 2 == 0);
            std::iter::repeat_n(filled.then_some(i), usize::from(d - b'0'))
        });
    
    let mut accum = 0;
    let mut index = 0;
    'outer: while let Some(m_block) = iter.next() {
        let bl = match m_block {
            Some(bl) => bl,
            None => loop {
                match iter.next_back() {
                    Some(Some(n)) => break n,
                    Some(None) => continue,
                    None => break 'outer,
                }
            },
        };
        
        accum += index * bl;
        index += 1;
    }
    
    accum
}

pub fn part2(input: &str) -> usize {
    let mut data = input.bytes().enumerate()
        .map(|(ind, d)| {
            let (i, filled) = (ind / 2, ind % 2 == 0);
            (usize::from(d - b'0'), filled.then_some(i))
        })
        .collect::<Vec<_>>();

    let mut i = 0;
    while let Some(&(di_size, di_block)) = data.get(i) {
        if di_block.is_none() {
            if let Some(j) = data[i..].iter().rposition(|&(dj_size, dj_block)| dj_block.is_some() && dj_size <= di_size) {
                let (dj_size, dj_block) = &mut data[i + j];
                let new_data = [(*dj_size, dj_block.take()), (di_size - *dj_size, di_block)];
                data.splice(i..=i, new_data);
            }
        }

        i += 1;
    }

    data.into_iter()
        .fold((0, 0), |(sum, i), (size, block)| (sum + block.unwrap_or(0) * size * (size + 2 * i - 1) / 2, i + size))
        .0
}

#[cfg(test)]
mod test {
    #[test]
    fn d9_correct() {
        let input = std::fs::read_to_string("inputs/09.txt").unwrap();
        assert_eq!(super::part1(&input), 6299243228569);
        assert_eq!(super::part2(&input), 6326952672104);
    }
}
