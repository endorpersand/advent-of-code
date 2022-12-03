use std::fs;
use std::str::FromStr;

// PART A(ish)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum PlayerMove {
    Rock, Paper, Scissors
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum GameWinner {
    Opp, Draw, You
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Game1(PlayerMove /* opp */, PlayerMove /* you */);

trait Enumerable {
    fn index(&self) -> usize;
    fn from_index(idx: usize) -> Self;
}

macro_rules! enumerable {
    ($($enum:ident {$($a:ident => $b:expr),*}),*) => {
        $(
            impl Enumerable for $enum {
                fn index(&self) -> usize {
                    match self {
                        $($enum::$a => $b),*
                    }
                }
            
                fn from_index(idx: usize) -> Self {
                    match idx {
                        $($b => $enum::$a),*,
                        _ => unreachable!()
                    }
                }
            }
        )*
    }
}

enumerable! {
    PlayerMove {
        Rock => 0,
        Paper => 1,
        Scissors => 2
    },
    GameWinner {
        Draw => 0,
        Opp => 1,
        You => 2
    }
}

impl PlayerMove {
    fn score(&self) -> usize {
        self.index() + 1
    }
}

impl GameWinner {
    fn score(&self) -> usize {
        match self {
            GameWinner::Opp  => 0,
            GameWinner::Draw => 3,
            GameWinner::You  => 6,
        }
    }
}

impl Game1 {
    fn winner(&self) -> GameWinner {
        // Rock + Rock:     0 - 0 === 0 (mod 3)
        // Rock + Paper:    0 - 1 === 2 (mod 3)
        // Rock + Scissors: 0 - 2 === 1 (mod 3)

        GameWinner::from_index(
            (3 + self.0.index() - self.1.index()).rem_euclid(3)
        )
    }

    fn score(&self) -> usize {
        self.winner().score() + self.1.score()
    }
}

impl FromStr for PlayerMove {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "A" | "X" => Ok(PlayerMove::Rock),
            "B" | "Y" => Ok(PlayerMove::Paper),
            "C" | "Z" => Ok(PlayerMove::Scissors),
            _ => Err(())
        }
    }
}

impl FromStr for Game1 {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut moves = s.split(" ");

        let opp = moves.next().unwrap().parse()?;
        let you = moves.next().unwrap().parse()?;

        Ok(Game1(opp, you))
    }
}

// PART B
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Game2(PlayerMove /* opp */, GameWinner /* you */);

impl FromStr for Game2 {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut moves = s.split(" ");

        let opp = moves.next().unwrap().parse()?;
        let you = moves.next().unwrap().parse()?;
        
        Ok(Game2(opp, you))
    }
}

impl FromStr for GameWinner {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "X" => Ok(GameWinner::Opp),
            "Y" => Ok(GameWinner::Draw),
            "Z" => Ok(GameWinner::You),
            _ => Err(())
        }
    }
}

impl Game2 {
    fn your_move(&self) -> PlayerMove {
        // opp - you = GameWinner, so
        // opp - GameWinner = you

        PlayerMove::from_index(
            (3 + self.0.index() - self.1.index()).rem_euclid(3)
        )
    }

    fn score(&self) -> usize {
        self.1.score() + self.your_move().score()
    }
}

fn main() {
    let input = fs::read_to_string("inputs/2.txt").unwrap();

    // PART A
    let values: Vec<_> = input.lines()
        .map(|line| line.parse::<Game1>().map(|g| g.score()))
        .collect::<Result<_, _>>()
        .unwrap();
    
    println!("{}", values.iter().sum::<usize>());

    // PART B
    let values: Vec<_> = input.lines()
        .map(|line| line.parse::<Game2>().map(|g| g.score()))
        .collect::<Result<_, _>>()
        .unwrap();

    println!("{}", values.iter().sum::<usize>());
}