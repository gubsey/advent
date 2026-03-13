use std::{
    cmp::Reverse,
    collections::{HashMap, HashSet, hash_map::OccupiedEntry},
    hash::Hash,
    io::stdin,
    iter::once,
};

use InOut::*;

fn main() {
    let points = stdin()
        .lines()
        .map(|s| {
            s.unwrap()
                .split(',')
                .map(|x| x.parse::<usize>().unwrap())
                .collect::<Vec<_>>()
        })
        .map(|v| [v[0], v[1]])
        .collect::<Vec<_>>();

    let vectors = points
        .iter()
        .copied()
        .enumerate()
        .flat_map(|(i, [ax, ay])| {
            points[i + 1..]
                .iter()
                .copied()
                .map(move |[bx, by]| [ax.abs_diff(bx) + 1, ay.abs_diff(by) + 1])
        })
        .collect::<Vec<_>>();

    let p1 = vectors.iter().copied().map(|[x, y]| x * y).max().unwrap();

    let [min_x, min_y, max_x, max_y] = points.iter().copied().fold(
        [usize::MAX, usize::MAX, 0, 0],
        |[mix, miy, max, may], [x, y]| [mix.min(x), miy.min(y), max.max(x), may.max(y)],
    );

    let mut walls = HashSet::new();

    points
        .array_windows()
        .chain(once(&[points.last().copied().unwrap(), points[0]]))
        .for_each(|&[[ax, ay], [bx, by]]| {
            if ax == bx {
                let [min, max] = [ay.min(by), ay.max(by)];
                for y in min..=max {
                    walls.insert([ax, y]);
                }
            } else {
                let [min, max] = [ax.min(bx), ax.max(bx)];
                for x in min..=max {
                    walls.insert([x, ay]);
                }
            }
        });

    let mut inner_boxes = points
        .iter()
        .copied()
        .enumerate()
        .flat_map(|(i, [ax, ay])| {
            points[i + 1..]
                .iter()
                .copied()
                .filter(move |[bx, by]| ax != *bx && ay != *by)
                .map(move |[bx, by]| {
                    (
                        [ax.min(bx), ay.min(by)],
                        [ax.max(bx), ay.max(by)],
                        (ax.abs_diff(bx) + 1) * (ay.abs_diff(by) + 1),
                    )
                })
        })
        .collect::<Vec<_>>();

    inner_boxes.sort_by_key(|x| Reverse(x.2));
    let mut in_out_map = HashMap::new();
    let p2 = inner_boxes
        .into_iter()
        .find_map(|([ax, ay], [bx, by], size)| {
            if (InOutChecker {
                walls: &walls,
                map: &mut in_out_map,
                watching: &mut HashSet::new(),
                min_x,
                min_y,
                max_x,
                max_y,
            })
            .is_in(ax + 1, ay + 1)
                == Some(In)
                && (ax + 1..bx)
                    .flat_map(|x| [[x, ay + 1], [x, by - 1]])
                    .chain((ay + 2..by - 1).flat_map(|y| [[ax + 1, y], [bx - 1, y]]))
                    .all(|x| !walls.contains(&x))
            {
                /*
                for y in min_y - 1..=max_y + 1 {
                    for x in min_x - 1..=max_x + 1 {
                        if (ax..=bx).contains(&x) && (ay..=by).contains(&y) {
                            print!("b")
                        } else if in_out_map.get(&[x, y]) == Some(&In) {
                            print!("i")
                        } else if walls.contains(&[x, y]) {
                            print!("w")
                        } else {
                            print!(".")
                        }
                    }
                    println!()
                }
                */
                Some(size)
            } else {
                None
            }
        })
        .unwrap();

    println!("p1: {p1}\np2: {p2}");
}

#[derive(Clone, Copy, Eq, PartialEq)]
enum InOut {
    In,
    Out,
}

struct InOutChecker<'a> {
    walls: &'a HashSet<[usize; 2]>,
    map: &'a mut HashMap<[usize; 2], InOut>,
    watching: &'a mut HashSet<[usize; 2]>,
    min_x: usize,
    min_y: usize,
    max_x: usize,
    max_y: usize,
}

impl<'a> InOutChecker<'a> {
    fn is_in(&mut self, x: usize, y: usize) -> Option<InOut> {
        if self.watching.contains(&[x, y]) || self.walls.contains(&[x, y]) {
            return None;
        }
        if let Some(&r) = self.map.get(&[x, y]) {
            return Some(r);
        }

        self.watching.insert([x, y]);
        if x == self.min_x || x == self.max_x || y == self.min_y || y == self.max_y {
            self.map.insert([x, y], Out);
            Some(Out)
        } else {
            for [ax, ay] in [[x - 1, y], [x + 1, y], [x, y - 1], [x, y + 1]] {
                if let Some(r) = self.is_in(ax, ay) {
                    self.map.insert([x, y], r);
                    return Some(r);
                }
            }
            self.map.insert([x, y], In);
            Some(In)
        }
    }
}

/* hack
he idea is not to divide them by some number but remap them. 
like take all the x coordinates, sort and unique them then, 
map them to an increasing integer sequence where you also take care of gaps.

eg:

[1, 2, 4, 6, 9, 10, 16] -> [0, 1, 3, 5, 7, 8, 10]
 */