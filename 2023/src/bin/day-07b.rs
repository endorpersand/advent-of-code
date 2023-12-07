use std::collections::HashMap;

use logos::Logos;

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

#[derive(Debug, Logos, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
enum Card {
    #[token("J")]
    J,
    #[token("2")]
    _2,
    #[token("3")]
    _3,
    #[token("4")]
    _4,
    #[token("5")]
    _5,
    #[token("6")]
    _6,
    #[token("7")]
    _7,
    #[token("8")]
    _8,
    #[token("9")]
    _9,
    #[token("T")]
    T,
    #[token("Q")]
    Q,
    #[token("K")]
    K,
    #[token("A")]
    A
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
    let mut lx = Card::lexer(hand);
    Hand(std::array::from_fn(|_| lx.next().unwrap().unwrap()))
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