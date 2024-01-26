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

type FF = Box<dyn Fn(&str) -> String + 'static>;

#[derive(Default)]
pub struct AdventParts {
    example: bool,
    p1: Option<FF>,
    p2: Option<FF>,
}

#[allow(private_bounds)]
impl AdventParts {
    pub fn example(self) -> Self {
        Self {
            example: true,
            ..self
        }
    }

    pub fn part1(self, f: impl Fn(&str) -> String + 'static) -> Self {
        Self {
            p1: Some(Box::new(f)),
            ..self
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
        let input = fs::read_to_string(file).expect(&format!("Could not read {file:?}"));

        self.p1.map(|f| f.run(&input, 1));
        self.p2.map(|f| f.run(&input, 2));
    }
}
