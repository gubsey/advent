#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Card {
    Ace,
    King,
    Queen,
    Jack,
    Ten,
    Nine,
    Eight,
    Seven,
    Six,
    Five,
    Four,
    Three,
    Two,
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

#[derive(Debug)]
pub struct Hand(Vec<Card>);

impl From<&str> for Hand {
    fn from(value: &str) -> Self {
        Hand(value.chars().map(Card::from).collect())
    }
}
