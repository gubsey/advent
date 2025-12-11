use std::{io::stdin, iter::repeat_with};

fn main() {
    let mut lines = stdin().lines().map(Result::unwrap).collect::<Vec<_>>();
    let ops = lines.pop().unwrap();
    let ops = ops.split(' ').filter(|s| !s.is_empty()).collect::<Vec<_>>();
    let nums = lines
        .into_iter()
        .map(|s| {
            s.split(' ')
                .filter(|s| !s.is_empty())
                .map(|s| s.parse::<usize>().unwrap())
                .collect::<Vec<_>>()
        })
        .fold(
            repeat_with(Vec::new).take(ops.len()).collect::<Vec<_>>(),
            |mut acc, line| {
                for (i, c) in line.into_iter().enumerate() {
                    acc[i].push(c);
                }
                acc
            },
        );

    let p1 = ops
        .into_iter()
        .zip(nums)
        .inspect(|x| println!("{x:?}"))
        .map(|(o, v)| match o {
            "+" => v.into_iter().sum::<usize>(),
            "*" => v.into_iter().product(),
            _ => panic!(),
        })
        .sum::<usize>();

    println!("p1: {p1}");
}
