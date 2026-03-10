use std::{
    collections::HashSet,
    io::{Read, stdin},
};

fn main() {
    let mut inp = String::new();
    stdin().read_to_string(&mut inp).unwrap();
    let (rules_str, updates_str) = inp.split_once("\n\n").unwrap();
    let rules: HashSet<(usize, usize)> = rules_str
        .lines()
        .map(|x| x.split_once('|').unwrap())
        .map(|(a, b)| (a.parse().unwrap(), b.parse().unwrap()))
        .collect();
    let mut updates: Vec<Vec<usize>> = updates_str
        .lines()
        .map(|l| l.split(',').map(|s| s.parse().unwrap()).collect())
        .collect();
    let p1 = updates
        .iter()
        .filter_map(|v| {
            for i in 0..v.len() {
                let k = v[i];
                let s = &v[i + 1..];
                for x in s {
                    if rules.contains(&(*x, k)) {
                        return None;
                    }
                }
            }
            Some(v[v.len() / 2])
        })
        .sum::<usize>();
    let p2 = updates
        .iter_mut()
        .filter_map(|u| {
            let mut wrong = false;
            let len = u.len();
            for [i, j] in (0..len - 1).flat_map(move |i| (i + 1..len).map(move |j| [i, j])) {
                let a = u[i];
                let b = u[j];
                if rules.contains(&(b, a)) {
                    u.swap(i, j);
                    wrong = true;
                }
            }
            wrong.then(|| u[u.len() / 2])
        })
        .sum::<usize>();
    println!("p1: {p1}\np2: {p2}");
}
