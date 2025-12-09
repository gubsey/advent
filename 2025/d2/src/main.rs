use std::{collections::HashSet, io::stdin};

fn main() {
    let ids = stdin()
        .lines()
        .next()
        .unwrap()
        .unwrap()
        .split(',')
        .map(|s| s.split_once('-').unwrap())
        .flat_map(|(a, b)| a.parse::<u64>().unwrap()..=b.parse().unwrap())
        .collect::<Vec<_>>();

    let p1: u64 = ids
        .iter()
        .filter(|&&x| {
            let digits = f64::log10(x as f64).ceil() as usize;
            if digits & 1 == 1 {
                return false;
            }
            let b = x.to_string().into_bytes();
            if b[0..digits / 2] != b[digits / 2..] {
                return false;
            }
            true
        })
        .inspect(|x| println!("{x}"))
        .sum();

    let p2: u64 = ids
        .iter()
        .filter(|&&x| {
            let digits = f64::log10(x as f64).ceil() as usize;
            let bytes = x.to_string().into_bytes();
            for n in (1..=digits / 2).rev() {
                let chunks = bytes.chunks(n).collect::<HashSet<_>>();
                if chunks.len() == 1 {
                    return true;
                }
            }
            false
        })
        .inspect(|x| println!("{x}"))
        .sum();
    println!("p1: {p1}\np2: {p2}");
}
