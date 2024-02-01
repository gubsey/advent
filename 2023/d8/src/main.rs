use std::collections::HashMap;

use advent_lib::AdventParts;

fn main() {
    AdventParts::part1(part1).part2(part2).run();
}

fn parse_input(input: &str) -> (impl Iterator<Item = Turn>, HashMap<&str, (&str, &str)>) {
    let mut lines = input.lines();
    let directions = lines
        .next()
        .expect("Could not read first line")
        .chars()
        .map(Turn::from)
        .collect::<Vec<_>>()
        .into_iter()
        .cycle();

    lines.next();
    let map = lines
        .map(|s| s.split_once(" = ").unwrap())
        .map(|(a, b)| (a, b.trim_matches(&['(', ')']).split_once(", ").unwrap()))
        .collect::<HashMap<_, _>>();
    (directions, map)
}

fn part1(input: &str) -> String {
    let (mut directions, map) = parse_input(input);
    let mut current = map.get("AAA").unwrap();

    (1..)
        .find(|_| {
            let next = match directions.next().unwrap() {
                Turn::Left => current.0,
                Turn::Right => current.1,
            };

            current = map.get(next).unwrap();

            next == "ZZZ"
        })
        .unwrap()
        .to_string()
}

fn part2(input: &str) -> String {
    let (directions, map) = parse_input(input);

    let mut ghosts: Vec<_> = map
        .keys()
        .filter(|s| s.ends_with('A'))
        .map(|s| (*s, Err(*s)))
        .collect();

    directions.zip(1..).find(|(dir, i)| {
        ghosts.iter_mut().map(|(_, b)| b).for_each(|x| {
            if let Err(curr) = x {
                let next = match dir {
                    Turn::Left => map[*curr].0,
                    Turn::Right => map[*curr].1,
                };

                if next.ends_with('Z') {
                    *x = Ok(*i)
                } else {
                    *x = Err(next)
                }
            }
        });

        ghosts.iter().all(|g| g.1.is_ok())
    });

    let lcm = |nums: &[u64]| match nums {
        [a, xs @ ..] => {
            let b = lcm(xs);
            *a * (b / gcd(*a, b))
        }
        [] => 1,
    };

    let nums = ghosts
        .into_iter()
        .map(|(_, b)| b.unwrap())
        .collect::<Vec<_>>();

    let lcm = lcm(&nums);

    lcm.to_string()
}

#[derive(Debug, Clone, Copy)]
enum Turn {
    Left,
    Right,
}

impl From<char> for Turn {
    fn from(value: char) -> Self {
        match value {
            'L' => Self::Left,
            'R' => Self::Right,
            _ => panic!("{value:?} is not a valid `Turn`"),
        }
    }
}

const fn gcd(a: u64, b: u64) -> u64 {
    if b == 0 {
        a
    } else {
        gcd(b, a % b)
    }
}

const fn lcm(nums: &[u64]) -> u64 {
    let &[a, ref xs @ ..] = nums else { return 1 };

    let b = lcm(xs);
    a * (b / gcd(a, b))
}
