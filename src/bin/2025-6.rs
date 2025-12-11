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

    let p1 = ops.iter().copied().zip(nums).map(f).sum::<usize>();

    let nums2 = lines
        .iter()
        .enumerate()
        .fold(
            repeat_with(|| vec![' '; lines.len()])
                .take(lines[0].len())
                .collect::<Vec<_>>(),
            |mut acc, (x, line)| {
                for (y, c) in line.char_indices() {
                    acc[y][x] = c;
                }
                acc
            },
        )
        .into_iter()
        .map(|v| v.into_iter().collect::<String>().trim().parse::<usize>())
        .collect::<Vec<_>>();
    let nums2 = nums2
        .split(|r| r.is_err())
        .map(|x| x.iter().cloned().map(Result::unwrap).collect::<Vec<_>>())
        .collect::<Vec<_>>();
    let p2 = ops.into_iter().zip(nums2).map(f).sum::<usize>();

    println!("p1: {p1}\np2: {p2}");
}
