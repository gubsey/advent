use std::io::stdin;

fn main() {
    let batteries = stdin()
        .lines()
        .map(|x| {
            x.unwrap()
                .into_bytes()
                .into_iter()
                .map(|b| (b - b'0') as usize)
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();

    let p1 = batteries
        .iter()
        .map(|nums| max_joltage(nums, 2))
        .inspect(|x| println!("{x}"))
        .sum::<usize>();

    println!();

    let p2 = batteries
        .iter()
        .map(|nums| max_joltage(nums, 12))
        .inspect(|x| println!("{x}"))
        .sum::<usize>();

    println!("p1: {p1}\np2: {p2}");
}

fn max_joltage(batteries: &[usize], digits: usize) -> usize {
    let mut jltg = 0;
    let mut start = 0;

    for digits_remaining in (0..digits).rev() {
        let (i, n) = batteries[start..batteries.len() - digits_remaining]
            .iter()
            .enumerate()
            .rev()
            .max_by_key(|x| *x.1)
            .unwrap();
        start += i + 1;
        jltg *= 10;
        jltg += n;
    }

    jltg
}
