use std::{str::FromStr, vec};
use tap::Pipe;

type Num = i32;

fn main() {
    advent_lib::AdventParts::part1(p1).part2(p2).run();
}

fn p1(input: &str) -> String {
    solve(input, |x| *x.last().unwrap(), |a, b| a + b).to_string()
}

fn p2(input: &str) -> String {
    solve(input, |x| x[0], |a, b| b - a).to_string()
}

fn solve(
    input: &str,
    map: impl Fn(Vec<i32>) -> i32 + Copy,
    reduce: impl Fn(i32, i32) -> i32 + Copy,
) -> i32 {
    get_reports(input)
        .into_iter()
        .map(|x| x.0.into_iter().map(map).rev().reduce(reduce).unwrap())
        .sum()
}

fn get_reports(input: &str) -> Vec<Report> {
    input
        .lines()
        .map(str::parse)
        .collect::<Result<_, _>>()
        .unwrap()
}

#[derive(Debug)]
struct Report(Vec<Vec<Num>>);

impl Report {
    fn new(v: Vec<Num>) -> Self {
        let mut r = vec![v];

        loop {
            let next = r.last().unwrap().windows(2).fold(Vec::new(), |mut acc, x| {
                acc.push(x[1] - x[0]);
                acc
            });

            if !next.iter().all(|x| *x == 0) {
                r.push(next);
            } else {
                break;
            };
        }

        Report(r)
    }
}

impl FromStr for Report {
    type Err = <Num as FromStr>::Err;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        s.split_whitespace()
            .map(|x| x.parse())
            .collect::<Result<Vec<_>, _>>()?
            .pipe(Report::new)
            .pipe(Ok)
    }
}
