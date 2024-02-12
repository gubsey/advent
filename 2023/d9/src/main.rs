use std::{num::ParseIntError, path::Iter, str::FromStr, vec};
use tap::Pipe;

type Num = i32;

fn main() {
    advent_lib::AdventParts::part1(p1)
        .part2(p2)
        //.example()
        .run();
}

fn p1(input: &str) -> String {
    let reports: Vec<Report> = input
        .lines()
        .map(str::parse)
        .collect::<Result<_, _>>()
        .unwrap();

    reports
        .into_iter()
        .map(|mut x| x.next().unwrap())
        .sum::<i32>()
        .to_string()
}

fn p2(input: &str) -> String {
    let reports: Vec<Report> = input
        .lines()
        .map(str::parse)
        .collect::<Result<_, _>>()
        .unwrap();

    reports
        .into_iter()
        .map(|x| {
            x.0.into_iter()
                .map(|x| x[0])
                .rev()
                .reduce(|a, b| b - a)
                .unwrap()
        })
        .sum::<i32>()
        .to_string()
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

impl Iterator for Report {
    type Item = Num;
    fn next(&mut self) -> Option<Self::Item> {
        let mut last = 0;

        for v in self.0.iter_mut().rev() {
            last += v.last().unwrap();
            v.push(last);
        }

        Some(last)
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
