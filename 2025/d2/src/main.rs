use std::io::stdin;

fn main() {
    let p1: u64 = stdin()
        .lines()
        .next()
        .unwrap()
        .unwrap()
        .split(',')
        .map(|s| s.split_once('-').unwrap())
        .flat_map(|(a, b)| a.parse::<u64>().unwrap()..=b.parse().unwrap())
        .filter(|x| {
            let digits = f64::log10(*x as f64).ceil() as usize;
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
    println!("p1: {p1}");
}
