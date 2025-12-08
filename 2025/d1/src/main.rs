use std::io::stdin;

fn main() {
    let inp = stdin()
        .lines()
        .map(Result::unwrap)
        .map(|s| (&s[0..=0] == "R", s[1..].parse::<i32>().unwrap()))
        .map(|(a, b)| if a { b } else { -b })
        .collect::<Vec<_>>();
    let p1 = inp
        .iter()
        .copied()
        .scan(50, |acc, x| {
            *acc += x;
            *acc = acc.rem_euclid(100);
            Some(*acc)
        })
        .filter(|x| *x == 0)
        .count();
    let p2 = inp
        .iter()
        .copied()
        .scan(50, |acc, x| {
            let r = *acc + x;
            let i = if r < *acc {
                (r..=*acc).rev().skip(1).collect::<Vec<_>>().into_iter()
            } else {
                (*acc..=r).skip(1).collect::<Vec<_>>().into_iter()
            }
            .map(|x| x.rem_euclid(100));
            *acc = r.rem_euclid(100);
            Some(i)
        })
        .flatten()
        .filter(|x| *x == 0)
        .count();
    println!("p1: {p1}\np2: {p2}");
}
