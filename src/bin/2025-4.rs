use std::{
    collections::{HashMap, HashSet},
    io::stdin,
};

fn main() {
    let mut map = stdin()
        .lines()
        .map(Result::unwrap)
        .enumerate()
        .flat_map(|(y, row)| {
            row.chars()
                .enumerate()
                .filter_map(move |(x, c)| {
                    if c == '@' {
                        Some(((x as i32, y as i32), HashSet::new()))
                    } else {
                        None
                    }
                })
                .collect::<HashMap<_, _>>()
        })
        .collect::<HashMap<_, _>>();

    let keys = map.keys().copied().collect::<HashSet<_>>();

    for ((x, y), set) in map.iter_mut() {
        for y2 in -1..2 {
            for x2 in -1..2 {
                if x2 == 0 && y2 == 0 {
                    continue;
                }
                let n = (*x + x2, *y + y2);
                if keys.contains(&n) {
                    set.insert(n);
                }
            }
        }
    }

    let p1 = map.values().filter(|x| x.len() < 4).count();

    loop {
        let mut under_4 = vec![];
        for (k, v) in map.iter() {
            if v.len() < 4 {
                under_4.push(*k);
            }
        }

        if under_4.is_empty() {
            break;
        }

        for r in under_4 {
            let set = map.remove(&r).unwrap();
            for k in set {
                map.get_mut(&k).unwrap().remove(&r);
            }
        }
    }

    println!("p1: {p1}\np2: {}", keys.len() - map.len());
}
