fn main() {
    println!("Hello, world!");
}

struct Record {
    time: u64,
    distance: u64,
}

impl Record {
    fn wr_count(&self) -> u64 {
        let q = self.distance / self.time;
        let q_min = q.min(self.time - q);

        for i in q_min.. {
            let chiral = self.time -i;
            if i * (self.time-i) > self.distance {
                return 
            }
        }

        0
    }
}

fn part1(input: &str) -> String {
    let mut line_vals = input.lines().map(|x| {
        x.split_ascii_whitespace()
            .skip(1)
            .map(|x| x.parse().unwrap())
    });

    line_vals
        .next()
        .unwrap()
        .zip(line_vals.next().unwrap())
        .map(|(time, distance)| Record { time, distance });
    "".to_string()
}
