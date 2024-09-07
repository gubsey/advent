use std::{
    io::{stdin, Read},
    iter::{Cycle, Enumerate},
    vec::IntoIter,
};

type Rock = Vec<Vec<bool>>;

fn main() {
    let stage = Stage::default();
}

fn parse_rocks(rock_strings: &str) -> Vec<Rock> {
    let mut rocks = vec![];
    for rock_str in rock_strings.split("\n\n") {
        let mut rock = vec![];
        for line in rock_str.lines() {
            let mut rock_line = vec![];
            for c in line.chars() {
                rock_line.push(c == '#');
            }
            rock.push(rock_line);
        }
        rocks.push(rock);
    }
    rocks
}

struct Stage {
    board: Vec<[bool; 7]>,
    rocks: Enumerate<Cycle<IntoIter<Rock>>>,
    moves: Cycle<IntoIter<Move>>,
}

impl Stage {
    fn mv(&mut self, mv: Move) {}
}

impl Default for Stage {
    fn default() -> Self {
        Stage {
            board: vec![],
            rocks: parse_rocks(include_str!("../rocks.txt"))
                .into_iter()
                .cycle()
                .enumerate(),
            moves: stdin()
                .lines()
                .next()
                .unwrap()
                .unwrap()
                .chars()
                .map(|c| match c {
                    '<' => Move::Left,
                    '>' => Move::Right,
                    _ => panic!("{c} is not a valid input"),
                })
                .collect::<Vec<_>>()
                .into_iter()
                .cycle(),
        }
    }
}

#[derive(Clone, Copy)]
enum Move {
    Left,
    Right,
}
