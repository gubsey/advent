use card::Hand;

type Bet = i32;

mod card;
fn main() {
    advent_lib::AdventParts::default()
        .part1(part1)
        .example()
        .run();
}

fn part1(input: &str) -> String {
    input
        .lines()
        .map(|x| x.split_once(' ').unwrap())
        .map(|(a, b)| (Hand::from(a), b.parse::<Bet>().unwrap()))
        .for_each(|x| println!("{x:?}"));

    String::new()
}
