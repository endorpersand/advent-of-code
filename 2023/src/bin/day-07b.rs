use std::collections::HashMap;

fn main() {
    let txt = std::fs::read_to_string("inputs/07.txt").unwrap();

    let mut hands: Vec<_> = txt.lines().map(parse).collect();
    hands.sort_by(|s1, s2| s1.hand.cmp(&s2.hand));
    
    let out: usize = hands.iter()
        .enumerate()
        .map(|(i, &State { bid, .. })| bid * (i + 1))
        .sum();
    println!("{out}");
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
#[repr(u8)]
#[allow(dead_code)]
enum Card {
    J  = b'J',
    _2 = b'2',
    _3 = b'3',
    _4 = b'4',
    _5 = b'5',
    _6 = b'6',
    _7 = b'7',
    _8 = b'8',
    _9 = b'9',
    T  = b'T',
    Q  = b'Q',
    K  = b'K',
    A  = b'A',
}
impl PartialOrd for Card {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for Card {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        fn key(c: &Card) -> impl Ord {
            match c {
                Card::J  => 0,
                Card::_2 => 1,
                Card::_3 => 2,
                Card::_4 => 3,
                Card::_5 => 4,
                Card::_6 => 5,
                Card::_7 => 6,
                Card::_8 => 7,
                Card::_9 => 8,
                Card::T  => 9,
                Card::Q  => 10,
                Card::K  => 11,
                Card::A  => 12
            }
        }

        key(self).cmp(&key(other))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Hand([Card; 5]);
impl PartialOrd for Hand {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.0.cmp(&other.0))
    }
}
impl Ord for Hand {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.get_type().cmp(&other.get_type())
            .then_with(|| self.0.cmp(&other.0))
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
enum HandType {
    HighCard,
    OnePair,
    TwoPair,
    ThreeOfAKind,
    FullHouse,
    FourOfAKind,
    FiveOfAKind
}

fn parse_hand(hand: &str) -> Hand {
    Hand(<[_; 5]>::try_from(unsafe {
        std::mem::transmute::<&[u8], &[Card]>(&hand.as_bytes()[..5])
    }).unwrap())
}
impl Hand {
    fn count(self) -> HashMap<Card, usize> {
        let mut m = HashMap::new();
        for c in self.0 {
            *m.entry(c).or_insert(0) += 1;
        }

        m
    }
    fn get_type(self) -> HandType {
        let mut count = self.count();
        let jcount = count.remove(&Card::J).unwrap_or(0);
        let mut cards: Vec<_> = count
            .into_iter()
            .collect();
        
        if cards.is_empty() {
            cards = vec![(Card::J, jcount)];
        } else {
            cards.sort_by_key(|&(_, ct)| std::cmp::Reverse(ct));
            cards[0].1 += jcount;
        }

        match cards[0].1 {
            5 => HandType::FiveOfAKind,
            4 => HandType::FourOfAKind,
            3 => match cards[1].1 {
                2 => HandType::FullHouse,
                1 => HandType::ThreeOfAKind,
                _ => unreachable!()
            },
            2 => match cards[1].1 {
                2 => HandType::TwoPair,
                1 => HandType::OnePair,
                _ => unreachable!()
            },
            1 => HandType::HighCard,
            _ => unreachable!()
        }
    }
}

#[derive(Debug)]
struct State {
    hand: Hand,
    bid: usize
}
fn parse(line: &str) -> State {
    let (hand, bid) = line.split_once(' ').unwrap();

    State {
        hand: parse_hand(hand),
        bid: bid.parse().unwrap()
    }
}