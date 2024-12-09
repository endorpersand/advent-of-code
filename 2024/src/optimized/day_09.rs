use std::collections::VecDeque;

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
        .collect::<VecDeque<_>>();

    let mut accum = 0;
    let mut index = 0;
    while let Some((mut size, m_block)) = data.pop_front() {
        let bl = match m_block {
            Some(bl) => bl,
            None => {
                while let Some((_, None)) = data.back() {
                    data.pop_back();
                }
                match data.iter_mut().rfind(|(size2, block2)| block2.is_some() && *size2 <= size) {
                    Some((size2, block2)) if size == *size2 => block2.take().unwrap_or(0),
                    Some((size2, block2)) => {
                        let (size2, block2) = (*size2, block2.take());
                        // allow blocks that haven't been allocated to be used
                        data.push_front((size - size2, None));
                        size = size2;
                        //
                        block2.unwrap_or(0)
                    }
                    _ => 0,
                }
            }
        };

        accum += bl * size * (size + 2 * index - 1) / 2;
        index += size;
    }

    accum
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
