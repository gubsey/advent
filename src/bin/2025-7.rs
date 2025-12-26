use std::{
    collections::{HashMap, HashSet},
    io::stdin,
};

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
    let start_v = start.iter().copied().next().unwrap();
    let sets = map.collect::<Vec<_>>();

    let p1_splits = sets
        .iter()
        .scan(start, |acc, v| {
            let mut v = v.clone();
            v.retain(|x| acc.contains(x));
            let splits = v.len();
            acc.retain(|x| !v.contains(x));
            acc.extend(v.into_iter().flat_map(|x| [x - 1, x + 1]));
            Some(splits)
        })
        .collect::<Vec<_>>();
    let mut timelines = HashMap::new();
    timelines.insert(start_v, 1i128);
    for set in sets {
        for item in set {
            if let Some(x) = timelines.remove(&item) {
                *timelines.entry(item - 1).or_default() += x;
                *timelines.entry(item + 1).or_default() += x;
            }
        }
    }
    println!(
        "p1: {}\np2: {}",
        p1_splits.iter().copied().sum::<usize>(),
        timelines.into_values().sum::<i128>()
    );
}
