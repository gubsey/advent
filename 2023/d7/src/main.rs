use card::Hand;
use fn_vec::FnVec;

use crate::card::Card;

type Bet = i64;

mod card;
mod fn_vec;

fn main() {
    advent_lib::AdventParts::default()
        .part1(|s| solve(s, false))
        .part2(|s| solve(s, true))
        //.example()
        .run();
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
        let c1 = a.0 .0.cmp(&b.0 .0);

        match c1 {
            std::cmp::Ordering::Equal => {
                if joker {
                    let joker_iter = |hand: &Hand| {
                        hand.slice()
                            .iter()
                            .map(|x| x.jack_to_joker())
                            .collect::<Vec<_>>()
                    };
                    joker_iter(&a.0 .1).cmp(&joker_iter(&b.0 .1))
                } else {
                    a.0 .1[..].iter().cmp(&b.0 .1[..])
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
            println!("{i}: {:?} {:?}", hand.0 .0, hand.0 .1.slice());
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
