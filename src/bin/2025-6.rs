use std::{io::stdin, iter::repeat_with};

fn main() {
    let mut lines = stdin().lines().map(Result::unwrap).collect::<Vec<_>>();
    let ops = lines.pop().unwrap();
    let ops = ops.split(' ').filter(|s| !s.is_empty()).collect::<Vec<_>>();
    let nums = lines
        .iter()
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

    fn f((o, v): (&str, Vec<usize>)) -> usize {
        match o {
            "+" => v.into_iter().sum::<usize>(),
            "*" => v.into_iter().product(),
            _ => panic!(),
        }
    }

    let p1 = ops.into_iter().zip(nums).map(f).sum::<usize>();

    let nums2 = lines.iter().enumerate().fold(
        repeat_with(|| vec![' '; lines.len()])
            .take(ops.len())
            .collect::<Vec<_>>(),
        |mut acc, (x, line)| {
            for (y, c) in line.char_indices() {
                acc[y][x] = c;
            }
            acc
        },
    );
    println!("{nums2:?}");

    println!("p1: {p1}");
}
