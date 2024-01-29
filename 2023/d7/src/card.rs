use std::{
    collections::HashMap,
    ops::{Index, RangeFull},
    str::FromStr,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Card {
    Two,
    Three,
    Four,
    Five,
    Six,
    Seven,
    Eight,
    Nine,
    Ten,
    Jack,
    Queen,
    King,
    Ace,
}

impl From<char> for Card {
    fn from(value: char) -> Self {
        use Card::*;
        "AKQJT"
            .chars()
            .chain(('2'..='9').rev())
            .zip([
                Ace, King, Queen, Jack, Ten, Nine, Eight, Seven, Six, Five, Four, Three, Two,
            ])
            .find(|(a, _)| *a == value)
            .unwrap()
            .1
    }
}

pub struct Hand(Vec<Card>);

impl Hand {
    pub fn slice(&self) -> &[Card] {
        &self[..]
    }

    pub fn iter(&self) -> impl Iterator<Item = &Card> {
        self.0.iter()
    }

    pub fn hash_map_count(&self) -> HashMap<&Card, usize> {
        self.iter().fold(HashMap::new(), |mut acc, x| {
            acc.entry(x).and_modify(|x| *x += 1).or_insert(1);
            //dbg!(&acc);
            acc
        })
    }
}

impl Index<usize> for Hand {
    type Output = Card;
    fn index(&self, index: usize) -> &Self::Output {
        &self.0[index]
    }
}

impl Index<RangeFull> for Hand {
    type Output = [Card];
    fn index(&self, _: RangeFull) -> &Self::Output {
        &self.0[..]
    }
}

impl FromStr for Hand {
    type Err = ();
    fn from_str(value: &str) -> Result<Self, ()> {
        Ok(Hand(value.chars().map(Card::from).collect()))
    }
}
