use std::{
    collections::{HashMap, HashSet},
    env::args,
    fmt::Debug,
    hash::Hash,
    io::stdin,
};

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct Point {
    x: i64,
    y: i64,
    z: i64,
}

impl Point {
    fn dist(self, rhs: Self) -> f64 {
        let big_d = (self.x - rhs.x).pow(2) + (self.y - rhs.y).pow(2) + (self.z - rhs.z).pow(2);
        (big_d as f64).sqrt()
    }
}

impl Debug for Point {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", [self.x, self.y, self.z])
    }
}

fn main() {
    let take_n = args()
        .nth(1)
        .expect("There must be at least one numeric argument")
        .parse::<usize>()
        .unwrap();
    let junction_boxes = stdin()
        .lines()
        .map(|s| {
            s.unwrap()
                .split(',')
                .map(|x| x.parse::<i64>().unwrap())
                .collect::<Vec<_>>()
        })
        .map(|v| Point {
            x: v[0],
            y: v[1],
            z: v[2],
        })
        .collect::<Vec<_>>();

    let mut distances = junction_boxes
        .iter()
        .copied()
        .enumerate()
        .flat_map(|(i, a)| {
            junction_boxes
                .iter()
                .copied()
                .skip(i + 1)
                .map(move |b| (a, b))
        })
        .map(|(a, b)| ((a, b), a.dist(b)))
        .filter(|x| x.1 != 0.)
        .collect::<Vec<_>>();

    distances.sort_by(|a, b| a.1.total_cmp(&b.1));

    let mut graph = Graph::default();
    for node in &junction_boxes {
        graph.add_node(node);
    }
    for ((a, b), _) in distances.iter().take(take_n) {
        let ap = graph.parent(a).unwrap();
        graph.set_parent(ap, b);
    }

    let circuits = graph.circuits();
    let mut circuit_sizes = circuits.values().copied().collect::<Vec<_>>();
    circuit_sizes.sort();

    let p1 = circuit_sizes.into_iter().rev().take(3).product::<usize>();

    println!("p1: {p1}");

    graph.reset_parents();
    let mut parents = junction_boxes.iter().collect::<HashSet<_>>();

    let mut p2 = 0;

    for ((a, b), _) in distances.iter() {
        let ap = graph.parent(a).unwrap();
        let bp = graph.parent(b).unwrap();
        if ap != bp {
            parents.remove(ap);
            if parents.len() == 1 {
                p2 = a.x * b.x;
                break;
            }
            graph.set_parent(ap, bp);
        }
    }

    println!("p2: {p2}");
}

#[derive(Default)]
struct Graph<'a>(HashMap<&'a Point, &'a Point>);

impl<'a> Graph<'a> {
    fn add_node(&mut self, node: &'a Point) {
        self.0.insert(node, node);
    }

    fn reset_parents(&mut self) {
        for (k, v) in self.0.iter_mut() {
            *v = *k
        }
    }

    fn set_parent(&mut self, node: &'a Point, newp: &'a Point) {
        let newp = self.parent(newp).unwrap();
        *self.0.get_mut(node).unwrap() = newp;
    }

    fn parent(&mut self, node: &'a Point) -> Option<&'a Point> {
        let p = self.0.get(node)?;
        if *p == node {
            Some(p)
        } else {
            let p = self.0.remove(node)?;
            let newp = self.parent(p)?;
            self.0.insert(node, newp);
            Some(newp)
        }
    }

    fn circuits(&mut self) -> HashMap<&'a Point, usize> {
        for k in self.0.keys().copied().collect::<Vec<_>>() {
            self.parent(k);
        }

        self.0.keys().fold(HashMap::new(), |mut acc, k| {
            let p = self.0[*k];
            acc.entry(p).and_modify(|x| *x += 1).or_insert(1);
            acc
        })
    }
}
