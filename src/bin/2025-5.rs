fn main() {
    let lines = std::io::stdin()
        .lines()
        .map_while(Result::ok)
        .collect::<Vec<String>>();
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

    while let Some([a, b]) = ranges
        .as_slice()
        .array_windows()
        .find(|&[[s1, e1], [s2, e2]]| s1.max(s2) <= e1.min(e2))
        .copied()
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
