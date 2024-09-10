use std::{
    fmt::Debug,
    io::stdin,
    iter::Cycle,
    ops::{Deref, DerefMut},
    vec::IntoIter,
};

use tap::Pipe;

fn main() {
    let mut stage = Stage::default();

    for _ in stage.moves.clone().take(20) {
        stage.next();
        stage.print();
        println!();
    }
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
        for (i, line) in rock_str.lines().enumerate() {
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
    board: Vec<[bool; 7]>,
    rocks: RocksIter,
    moves: Cycle<IntoIter<Move>>,
    falling: Option<(Rock, Vec2D)>,
    height: usize,
}

impl Stage {
    fn print(&self) {
        for (y, row) in self.board.iter().enumerate().rev() {
            for (x, cell) in row.iter().enumerate() {
                let mut c = if *cell { '#' } else { '.' };
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
    }
}

impl Iterator for Stage {
    type Item = ();
    fn next(&mut self) -> Option<()> {
        let float = match self.falling.as_mut() {
            Some(x) => x,
            None => {
                let r = (self.rocks.next().unwrap(), xy(2, self.height + 3));

                while self.board.len() < self.height + r.0.height + 3 {
                    self.board.push(Default::default());
                }

                self.falling = Some(r);
                self.falling.as_mut().unwrap()
            }
        };

        let mv = self.moves.next().unwrap();
        match mv {
            Move::Left => {
                if float.1.x > 0 {
                    float.1.x -= 1
                }
            }
            Move::Right => {
                if float.1.x + float.0.width < 7 {
                    float.1.x += 1
                }
            }
        }

        float.1.y -= 1;

        if float.1.y > self.height {
            return Some(())
        }

        
        if float.1.y > 0 {
            return Some(());
        }

        let float = self.falling.take().unwrap();
        let float_p = float.1;
        for (i, row) in float.0.into_iter().enumerate() {
            if i == float.0.height {
                break;
            }
            for (j, cell) in row.into_iter().enumerate() {
                if j == float.0.width {
                    break;
                };
                self.board[i + float_p.y][j + float_p.x] |= cell;
            }
        }

        self.height = float.0.height.max(self.height);

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

impl Default for Stage {
    fn default() -> Self {
        Stage {
            board: vec![],
            rocks: include_str!("../rocks.txt")
                .pipe(parse_rocks)
                .pipe(RocksIter::new),
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
            falling: None,
            height: 0,
        }
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
