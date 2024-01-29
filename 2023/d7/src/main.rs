use card::Hand;
use rules::Rules;

type Bet = i32;

mod card;
mod rules;

fn main() {
    advent_lib::AdventParts::default()
        .part1(part1)
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
fn rules<'a>() -> Rules<'a, Hand, Option<HandType>> {
    use HandType as E;
    let five_of_a_kind = |h: &Hand| {
        match h.slice() {
            [head, tail @ ..] => tail.iter().all(|x| x == head),
            _ => panic!(),
        }
        .then_some(E::FiveOfAKind)
    };
    let four_of_a_kind = |h: &Hand| {
        h.hash_map_count()
            .values()
            .any(|x| *x == 4)
            .then_some(E::FourOfAKind)
    };
    let full_house = |h: &Hand| {
        h.hash_map_count()
            .values()
            .all(|x| *x == 3 || *x == 2)
            .then_some(E::FullHouse)
    };
    let three_of_a_kind = |h: &Hand| {
        h.hash_map_count()
            .values()
            .any(|x| *x == 3)
            .then_some(E::ThreeOfAKind)
    };
    let two_pair = |h: &Hand| {
        (h.hash_map_count().values().filter(|x| **x == 2).count() == 2).then_some(E::TwoPair)
    };
    let one_pair = |h: &Hand| {
        h.hash_map_count()
            .values()
            .any(|x| *x == 2)
            .then_some(E::OnePair)
    };

    Rules::new()
        .rule(five_of_a_kind)
        .rule(four_of_a_kind)
        .rule(full_house)
        .rule(three_of_a_kind)
        .rule(two_pair)
        .rule(one_pair)
        .rule(|_| Some(E::HighCard))
}

fn part1(input: &str) -> String {
    let rules = rules();
    let mut hands = input
        .lines()
        .map(|x| x.split_once(' ').unwrap())
        .map(|(a, b)| (a.parse::<Hand>().unwrap(), b.parse::<Bet>().unwrap()))
        .map(|(a, b)| ((rules.find_unwrap(&a), a), b))
        .collect::<Vec<_>>();

    hands.sort_by(|a, b| {
        let c1 = a.0 .0.cmp(&b.0 .0);
        match c1 {
            std::cmp::Ordering::Equal => a.0 .1[..].iter().cmp(&b.0 .1[..]),
            _ => c1,
        }
    });

    hands
        .iter()
        .inspect(|x| println!("{:?}: {:?} {}", x.0 .0, &x.0 .1[..], x.1))
        .enumerate()
        .inspect(|(i, x)| print!("{} * {} = ", i + 1, x.1))
        .map(|(i, x)| (i + 1) as Bet * x.1)
        .inspect(|x| println!("{x}"))
        .sum::<i32>()
        .to_string()
}

#[test]
fn hand_type_test() {
    use HandType::*;

    let rules = rules();
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
    .for_each(|(a, b)| assert_eq!(rules.find_unwrap(&a), b))
}
