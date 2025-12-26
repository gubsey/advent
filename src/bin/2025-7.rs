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

    let mut timelines = vec![0u128; len + 1];
    timelines[start_v] = 1;
    for set in sets {
        for item in set {
            let x = timelines[item];
            timelines[item - 1] += x;
            timelines[item + 1] += x;
            timelines[item] = 0;
        }
    }
    let t_sum: u128 = timelines.iter().sum();
    println!(
        "p1: {}\np2: {}",
        p1_splits.iter().copied().sum::<usize>(),
        t_sum
    );
}
