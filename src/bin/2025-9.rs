use std::{collections::HashMap, io::stdin, iter::once};

fn main() {
    let points = stdin()
        .lines()
        .map(|s| {
            s.unwrap()
                .split(',')
                .map(|x| x.parse::<usize>().unwrap())
                .collect::<Vec<_>>()
        })
        .map(|v| [v[0], v[1]])
        .collect::<Vec<_>>();

    let vectors = points
        .iter()
        .copied()
        .enumerate()
        .flat_map(|(i, [ax, ay])| {
            points[i + 1..]
                .iter()
                .copied()
                .map(move |[bx, by]| [ax.abs_diff(bx) + 1, ay.abs_diff(by) + 1])
        })
        .collect::<Vec<_>>();

    let p1 = vectors.iter().copied().map(|[x, y]| x * y).max().unwrap();

    println!("p1: {p1}");
}
