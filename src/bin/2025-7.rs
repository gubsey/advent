use std::{collections::HashSet, io::stdin};

fn main() {
    let mut map = stdin()
        .lines()
        .map(Result::unwrap)
        .map(|s| {
            s.chars()
                .enumerate()
                .filter_map(|(i, c)| match c {
                    'S' | '^' => Some(i),
                    _ => None,
                })
                .collect::<HashSet<_>>()
        })
        .filter(|v| !v.is_empty());
    let start = map.next().unwrap();
    let beams = map
        .scan(start, |acc, mut v| {
            v.retain(|x| acc.contains(x));
            let splits = v.len();
            acc.retain(|x| !v.contains(x));
            acc.extend(v.into_iter().flat_map(|x| [x - 1, x + 1]));
            Some(splits)
        })
        .collect::<Vec<_>>();
    println!("{}", beams.iter().copied().sum::<usize>());
}
