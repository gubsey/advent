use std::io::stdin;

fn main() {
    let lines = stdin().lines().map(Result::unwrap).collect::<Vec<String>>();
    let mut stdin_split = lines.split(|x| x.is_empty());
    let mut ranges = stdin_split
        .next()
        .unwrap()
        .iter()
        .map(|x| x.split_once('-').unwrap())
        .map(|(a, b)| [a.parse::<u128>().unwrap(), b.parse().unwrap()])
        .collect::<Vec<_>>();
    let ids = stdin_split
        .next()
        .unwrap()
        .iter()
        .map(|x| x.parse().unwrap())
        .collect::<Vec<_>>();
    assert!(stdin_split.next().is_none());

    let p1 = ids
        .into_iter()
        .filter(|id| ranges.iter().any(|&[s, e]| (s..=e).contains(id)))
        .count();

    while let Some([&a, &b]) = ranges
        .as_slice()
        .pairs()
        .find(|&[&[s1, e1], &[s2, e2]]| s1.max(s2) <= e1.min(e2))
    {
        let s = a[0].min(b[0]);
        let e = a[1].max(b[1]);
        let new = [s, e];
        ranges.retain(|&x| x != a && x != b);
        ranges.push(new);
    }
    let p2 = ranges.iter().map(|&[a, b]| b - a + 1).sum::<u128>();

    println!("p1: {p1}\np2: {p2}");
}

struct Pairs<'a, T> {
    slice: &'a [T],
    a: usize,
    b: usize,
}

impl<'a, T> Iterator for Pairs<'a, T> {
    type Item = [&'a T; 2];

    fn next(&mut self) -> Option<Self::Item> {
        if self.a >= self.slice.len() {
            return None;
        }

        if self.b >= self.slice.len() {
            self.a += 1;
            self.b = self.a + 1;
        }

        let a = self.slice.get(self.a)?;
        let b = self.slice.get(self.b)?;

        self.b += 1;

        Some([a, b])
    }
}

trait ToPairs {
    type Out;
    fn pairs(&self) -> Pairs<'_, Self::Out>;
}

impl<T> ToPairs for &'_ [T] {
    type Out = T;
    fn pairs(&self) -> Pairs<'_, Self::Out> {
        Pairs {
            slice: self,
            a: 0,
            b: 1,
        }
    }
}
