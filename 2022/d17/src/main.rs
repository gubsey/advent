use std::{
    fmt::Debug,
    io::stdin,
    iter::Cycle,
    ops::{Deref, DerefMut},
    vec::IntoIter,
};

use tap::Pipe;

fn main() {
    let input = stdin().lines().next().unwrap().unwrap();
    let input = input.trim();
    let input_len = input.len();

    let mut stage = Stage::new(input);

    // part 1
    while stage.rocks.count <= 2022 {
        stage.next();
    }

    println!("{}", stage.height);

    // part 2 might not work for your input
    // part 2
    let mut stage = Stage::new(input);
    let mut a = 1600;

    for i in 0..99999 {
        if (i + a) % input_len == 0 {
            print!("{}, ", stage.height);
            a += 1;
        }
        stage.next();
    }
}

#[test]
fn example() {
    let mut stage = Stage::new(include_str!("../example.txt").trim());

    while stage.rocks.count <= 2022 {
        stage.next();
    }

    stage.print(10);

    assert_eq!(stage.height, 3068);
}

#[test]
fn falling_past() {
    let mut stage = Stage::new(
        ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<",
    );

    while stage.rocks.count <= 11 {
        stage.next();
    }
    stage.print(10);
    println!("{}", stage.height);
}

#[derive(Default, Clone, Copy)]
struct Rock {
    rock: [[bool; 4]; 4],
    width: usize,
    height: usize,
}

impl Deref for Rock {
    type Target = [[bool; 4]; 4];
    fn deref(&self) -> &Self::Target {
        &self.rock
    }
}

impl DerefMut for Rock {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.rock
    }
}

fn parse_rocks(rock_strings: &str) -> Vec<Rock> {
    let mut rocks = vec![];
    for rock_str in rock_strings.split("\n\n") {
        let mut rock = Rock::default();
        for (i, line) in rock_str.lines().rev().enumerate() {
            for (j, c) in line.char_indices() {
                rock[i][j] = c == '#';
                rock.width = j + 1;
            }
            rock.height = i + 1;
        }
        rocks.push(rock);
    }
    rocks
}

struct Stage {
    board: Vec<[usize; 7]>,
    rocks: RocksIter,
    moves: Cycle<IntoIter<Move>>,
    falling: Option<(Rock, Vec2D)>,
    height: usize,
}

impl Stage {
    fn print(&self, limit: usize) {
        for (y, row) in self
            .board
            .iter()
            .enumerate()
            .rev()
            .filter(|(_, x)| x.iter().any(|y| *y > 0))
            .take(limit)
        {
            for (x, cell) in row.iter().enumerate() {
                let mut c = if *cell == 0 {
                    '.'
                } else {
                    cell.to_string().as_bytes().last().copied().unwrap() as char
                };
                if let Some((r, p)) = self.falling {
                    if (p.x..p.x + 4).contains(&x)
                        && (p.y..p.y + 4).contains(&y)
                        && r[y - p.y][x - p.x]
                    {
                        c = '@';
                    }
                }
                print!("{c}");
            }
            println!(" | {y}");
        }
        println!();
    }

    fn set(&mut self) {
        let Some((rock, pos)) = self.falling.take() else {
            return;
        };
        for i in 0..rock.height {
            for j in 0..rock.width {
                if rock[i][j] {
                    self.board[i + pos.y][j + pos.x] = self.rocks.count;
                }
                self.height = self.height.max(rock.height + pos.y);
            }
        }
    }

    fn new(moves: &str) -> Self {
        Stage {
            board: vec![],
            rocks: include_str!("../rocks.txt")
                .pipe(parse_rocks)
                .pipe(RocksIter::new),
            moves: moves
                .chars()
                .map(|c| match c {
                    '<' => Move::Left,
                    '>' => Move::Right,
                    _ => panic!("{c} is not a valid input"),
                })
                .collect::<Vec<_>>()
                .into_iter()
                .cycle(),
            falling: None,
            height: 0,
        }
    }

    fn check_collision(&self) -> bool {
        let Some((rock, pos)) = self.falling else {
            return false;
        };
        for i in 0..rock.height {
            for j in 0..rock.width {
                if rock[i][j] && self.board[i + pos.y][j + pos.x] > 0 {
                    return true;
                }
            }
        }

        false
    }
}

impl Iterator for Stage {
    type Item = ();
    fn next(&mut self) -> Option<()> {
        if self.falling.is_none() {
            let r = (self.rocks.next()?, xy(2, self.height + 3));

            while self.board.len() < self.height + r.0.height + 3 {
                self.board.push(Default::default());
            }

            self.falling = Some(r);
        }

        let old_x = self.falling?.1.x;
        let mv = self.moves.next()?;
        match mv {
            Move::Left => {
                if self.falling?.1.x > 0 {
                    self.falling.as_mut()?.1.x -= 1
                }
            }
            Move::Right => {
                if self.falling?.1.x + self.falling?.0.width < 7 {
                    self.falling.as_mut()?.1.x += 1
                }
            }
        }

        if self.check_collision() {
            self.falling.as_mut()?.1.x = old_x;
        }

        if self.falling?.1.y == 0 {
            self.set();
            return Some(());
        }
        self.falling.as_mut()?.1.y -= 1;

        if self.falling?.1.y >= self.height {
            return Some(());
        }

        if self.check_collision() {
            self.falling.as_mut()?.1.y += 1;
            self.set();
        }

        Some(())
    }
}

struct RocksIter {
    rocks: Vec<Rock>,
    count: usize,
}

impl RocksIter {
    fn new(rocks: Vec<Rock>) -> Self {
        Self { rocks, count: 0 }
    }
}

impl Iterator for RocksIter {
    type Item = Rock;
    fn next(&mut self) -> Option<Self::Item> {
        let r = self.rocks[self.count % self.rocks.len()];
        self.count += 1;
        Some(r)
    }
}

#[derive(Clone, Copy, Debug)]
enum Move {
    Left,
    Right,
}

#[derive(Clone, Copy)]
struct Vec2D {
    x: usize,
    y: usize,
}

impl Debug for Vec2D {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}, {})", self.x, self.y)
    }
}

fn xy(x: usize, y: usize) -> Vec2D {
    Vec2D { x, y }
}
