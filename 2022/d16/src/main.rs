use std::collections::{HashMap, VecDeque};

type Num = i32;

fn main() {
    advent_lib::AdventParts::part1(p1).example().run();
}

fn p1(input: &str) -> String {
    let valves = valve_map(input);

    let mut current = valves.get_key_value("AA").unwrap();
    for i in 0..30 {
        let mut weights = HashMap::new();
        let mut queue = VecDeque::new();

        queue.push_back((*current.0, 1));

        while let Some((s, w)) = queue.pop_front() {
            if weights.keys().any(|x| x == &s) {
                continue;
            }

            let valve = valves.get(s).unwrap();

            let time_remaining = 30 - (i + w);

            weights.insert(s, time_remaining * valve.flow);
            valve
                .tunnels
                .iter()
                .for_each(|x| queue.push_back((x, w + 1)));
        }

        dbg!(weights);
        break;
    }

    "".to_string()
}

fn valve_map(input: &str) -> HashMap<&str, Valve> {
    input
        .lines()
        .map(|s| {
            let mut words = s.split_whitespace();
            (
                words.nth(1).unwrap(),
                Valve {
                    flow: words
                        .nth(2)
                        .unwrap()
                        .split(&['=', ';'])
                        .nth(1)
                        .unwrap()
                        .parse()
                        .unwrap(),
                    tunnels: words.skip(4).map(|x| x.trim_end_matches(',')).collect(),
                },
            )
        })
        .collect()
}

#[derive(Debug)]
struct Valve<'a> {
    flow: Num,
    tunnels: Vec<&'a str>,
}
