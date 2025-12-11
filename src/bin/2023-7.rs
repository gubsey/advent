use std::{
    collections::HashMap,
    io::{Read, stdin},
    ops::{Index, RangeFull},
    str::FromStr,
    vec,
};

type Bet = i64;

fn main() {
    let mut inp = String::new();
    stdin().read_to_string(&mut inp).unwrap();
    println!("p1: {}\np2: {}", solve(&inp, false), solve(&inp, true));
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum HandType {
    HighCard,
    OnePair,
    TwoPair,
    ThreeOfAKind,
    FullHouse,
    FourOfAKind,
    FiveOfAKind,
}

/// All rules assume a hand of exactly 5 cards
fn rules(joker: &'_ bool) -> FnVec<'_, Hand, Option<HandType>> {
    use HandType as E;
    let five_of_a_kind = |h: &Hand| {
        let [head, tail @ ..] = h.slice() else {
            panic!()
        };

        if !*joker {
            tail.iter().all(|x| x == head)
        } else {
            let mut h_map = h.hash_map_count();
            let jokers = h_map.remove(&Card::Jack).unwrap_or_default();
            h_map.values().any(|x| *x + jokers == 5) || jokers == 5
        }
        .then_some(E::FiveOfAKind)
    };
    let four_of_a_kind = |h: &Hand| {
        if !*joker {
            h.hash_map_count().values().any(|x| *x == 4)
        } else {
            let mut h_map = h.hash_map_count();
            let jokers = h_map.remove(&Card::Jack).unwrap_or_default();
            h_map.values().any(|x| *x + jokers == 4)
        }
        .then_some(E::FourOfAKind)
    };
    let full_house = |h: &Hand| {
        if *joker {
            // only the format of xxyyJ will result in full_house
            // xx3J will turn into 5x
            // xxyjj will become 4xy
            let mut h_map = h.hash_map_count();
            if h_map.remove(&Card::Jack).is_some() {
                h_map.values().all(|x| *x == 2)
            } else {
                h_map.values().all(|x| *x == 3 || *x == 2)
            }
        } else {
            h.hash_map_count().values().all(|x| *x == 3 || *x == 2)
        }
        .then_some(E::FullHouse)
    };
    let three_of_a_kind = |h: &Hand| {
        if *joker {
            let mut h_map = h.hash_map_count();
            let jokers = h_map.remove(&Card::Jack).unwrap_or_default();
            h_map.values().any(|x| *x + jokers == 3)
        } else {
            h.hash_map_count().values().any(|x| *x == 3)
        }
        .then_some(E::ThreeOfAKind)
    };
    let two_pair = |h: &Hand| {
        // joker will never result in two pair
        (h.hash_map_count().values().filter(|x| **x == 2).count() == 2).then_some(E::TwoPair)
    };
    let one_pair = |h: &Hand| {
        if *joker {
            h.hash_map_count()
                .iter()
                .any(|(k, v)| k == &&Card::Jack || v == &2)
        } else {
            h.hash_map_count().values().any(|x| *x == 2)
        }
        .then_some(E::OnePair)
    };

    FnVec::new()
        .rule(five_of_a_kind)
        .rule(four_of_a_kind)
        .rule(full_house)
        .rule(three_of_a_kind)
        .rule(two_pair)
        .rule(one_pair)
        .rule(|_| Some(E::HighCard))
}

fn sort_hands(hands: &mut [((HandType, Hand), i64)], joker: bool) {
    hands.sort_by(|a, b| {
        let c1 = a.0.0.cmp(&b.0.0);

        match c1 {
            std::cmp::Ordering::Equal => {
                if joker {
                    let joker_iter = |hand: &Hand| {
                        hand.slice()
                            .iter()
                            .map(|x| x.jack_to_joker())
                            .collect::<Vec<_>>()
                    };
                    joker_iter(&a.0.1).cmp(&joker_iter(&b.0.1))
                } else {
                    a.0.1[..].iter().cmp(&b.0.1[..])
                }
            }
            _ => c1,
        }
    });
}

fn solve(input: &str, joker: bool) -> String {
    let rules = rules(&joker);
    let mut hands = input
        .lines()
        .map(|x| x.split_once(' ').unwrap())
        .map(|(a, b)| (a.parse::<Hand>().unwrap(), b.parse::<Bet>().unwrap()))
        .map(|(a, b)| ((rules.find_unwrap(&a), a), b))
        .collect::<Vec<_>>();

    sort_hands(&mut hands, joker);
    if joker {
        for (i, hand) in hands.iter().enumerate() {
            println!("{i}: {:?} {:?}", hand.0.0, hand.0.1.slice());
        }
    }

    hands
        .iter()
        .enumerate()
        .map(|(i, x)| (i + 1) as Bet * x.1)
        .sum::<Bet>()
        .to_string()
}

#[test]
fn hand_type_test() {
    use HandType::*;

    let rules = rules(&false);

    [
        ("AAAAA", FiveOfAKind),
        ("KK3KK", FourOfAKind),
        ("Q4Q4Q", FullHouse),
        ("JJJ56", ThreeOfAKind),
        ("33242", TwoPair),
        ("22345", OnePair),
        ("62345", HighCard),
    ]
    .into_iter()
    .map(|(a, b)| (a.parse().unwrap(), b))
    .for_each(|(a, b)| assert_eq!(rules.find_unwrap(&a), b));
}

#[test]
fn hand_type_test_joker() {
    use HandType::*;
    let rules = rules(&true);

    [
        ("T55J5", FourOfAKind),
        ("KTJJT", FourOfAKind),
        ("3324J", ThreeOfAKind),
        ("225J6", ThreeOfAKind),
    ]
    .into_iter()
    .map(|(a, b)| (a.parse().unwrap(), b))
    .for_each(|(a, b)| assert_eq!(rules.find_unwrap(&a), b))
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Card {
    Joker,
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

impl Card {
    pub fn jack_to_joker(self) -> Self {
        if self == Self::Jack {
            Self::Joker
        } else {
            self
        }
    }
}

impl From<char> for Card {
    fn from(value: char) -> Self {
        use Card as C;
        "AKQJT"
            .chars()
            .chain(('2'..='9').rev())
            .zip([
                C::Ace,
                C::King,
                C::Queen,
                C::Jack,
                C::Ten,
                C::Nine,
                C::Eight,
                C::Seven,
                C::Six,
                C::Five,
                C::Four,
                C::Three,
                C::Two,
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

type BoxFn<'a, I, O> = Box<dyn Fn(&I) -> O + 'a>;

#[derive(Default)]
pub struct FnVec<'a, I, O>(Vec<BoxFn<'a, I, O>>);

impl<'a, I, O> FnVec<'a, I, O> {
    pub fn new() -> Self {
        FnVec(Vec::new())
    }

    pub fn rule(mut self, f: impl Fn(&I) -> O + 'a) -> Self {
        self.0.push(Box::new(f));
        self
    }

    pub fn iter(&self) -> impl Iterator<Item = &Box<dyn Fn(&I) -> O + 'a>> {
        self.0.iter()
    }
}

impl<'a, I, O> FnVec<'a, I, Option<O>> {
    pub fn find_unwrap(&self, val: &I) -> O {
        self.iter().find_map(|f| f(val)).unwrap()
    }
}

impl<'a, T, O> IntoIterator for FnVec<'a, T, O> {
    type Item = Box<dyn Fn(&T) -> O + 'a>;
    type IntoIter = vec::IntoIter<Self::Item>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}
