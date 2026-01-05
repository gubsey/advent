use std::{collections::HashSet, io::stdin};

fn main() {
    let mut len = 0;
    let mut map = stdin()
        .lines()
        .map(Result::unwrap)
        .map(|s| {
            s.chars()
                .enumerate()
                .filter_map(|(i, c)| match c {
                    'S' | '^' => Some(i),
                    _ => {
                        len = len.max(i);
                        None
                    }
                })
                .collect::<HashSet<_>>()
        })
        .filter(|v| !v.is_empty());
    let start = map.next().unwrap();
    let start_v = start.iter().copied().next().unwrap();
    let sets = map.collect::<Vec<_>>();

    let p1 = sets
        .clone()
        .into_iter()
        .scan(start, |acc, mut v| {
            v.retain(|x| acc.contains(x));
            let splits = v.len();
            acc.retain(|x| !v.contains(x));
            acc.extend(v.into_iter().flat_map(|x| [x - 1, x + 1]));
            Some(splits)
        })
        .sum::<usize>();

    let p2: u64 = sets
        .into_iter()
        .flatten()
        .fold(
            (0..=len)
                .map(|x| if x == start_v { 1 } else { 0 })
                .collect(),
            |mut timelines: Vec<u64>, item| {
                let x = timelines[item];
                timelines[item - 1] += x;
                timelines[item + 1] += x;
                timelines[item] = 0;
                timelines
            },
        )
        .into_iter()
        .sum();

    println!("p1: {}\np2: {}", p1, p2);
}
