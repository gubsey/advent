pub mod parser;

use std::{fs, time::Instant};

trait MyFn {
    fn run(self, s: &str, part: usize);
}

impl MyFn for FF {
    fn run(self, s: &str, part: usize) {
        let start = Instant::now();
        let out = self(s);
        let dur = start.elapsed();

        println!("Part {part}: {out}\n{dur:.2?}");
    }
}

pub trait AdventFn: Fn(&str) -> String + 'static {}

type FF = Box<dyn Fn(&str) -> String + 'static>;

#[derive(Default)]
pub struct AdventParts {
    pub p1: Option<FF>,
    pub p2: Option<FF>,
    pub example: bool,
}

#[allow(private_bounds)]
impl AdventParts {
    pub fn example(self) -> Self {
        Self {
            example: true,
            ..self
        }
    }

    pub fn part1(f: impl Fn(&str) -> String + 'static) -> Self {
        Self {
            p1: Some(Box::new(f)),
            p2: None,
            example: false,
        }
    }

    pub fn part2(self, f: impl Fn(&str) -> String + 'static) -> Self {
        Self {
            p2: Some(Box::new(f)),
            ..self
        }
    }

    pub fn run(self) {
        let file = if !self.example {
            "input.txt"
        } else {
            "example.txt"
        };
        let input = fs::read_to_string(file)
            .unwrap_or_else(|e| panic!("{}", format!("{e}\nCould not read {file:?}")));

        if let Some(f) = self.p1 {
            f.run(&input, 1)
        }
        if let Some(f) = self.p2 {
            f.run(&input, 2)
        }
    }
}
