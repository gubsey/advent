use std::{
    collections::HashSet,
    io::{stdin, Read},
};

fn main() {
    let mut grid = parse_voxels(
        stdin()
            .bytes()
            .map(|c| c.unwrap() as char)
            .collect::<String>()
            .as_ref(),
    );

    let mut sa = 0;

    for cell in &grid {
        for i in 0..3 {
            let mut pre = *cell;
            pre[i] -= 1;
            if !grid.contains(&pre) {
                sa += 1;
            }
            let mut next = *cell;
            next[i] += 1;
            if !grid.contains(&next) {
                sa += 1
            }
        }
    }

    println!("part 1: {sa}");

    let mut stack = vec![[0, 0, 0]];
    let mut used = HashSet::new();

    while let Some(cell) = stack.pop() {
        used.insert(cell);

        for i in 0..3 {
            let mut pre = cell;
            pre[i] -= 1;
            if pre[i] >= 0 && !grid.contains(&pre) && !used.contains(&pre) {
                stack.push(pre);
            }

            let mut next = cell;
            next[i] += 1;
            if next[i] <= 22 && !grid.contains(&next) && !used.contains(&next) {
                stack.push(next);
            }
        }
    }

    for x in 0..22 {
        for y in 0..22 {
            for z in 0..22 {
                if !used.contains(&[x, y, z]) {
                    grid.insert([x, y, z]);
                }
            }
        }
    }

    let mut sa = 0;

    for cell in &grid {
        for i in 0..3 {
            let mut pre = *cell;
            pre[i] -= 1;
            if !grid.contains(&pre) {
                sa += 1;
            }
            let mut next = *cell;
            next[i] += 1;
            if !grid.contains(&next) {
                sa += 1
            }
        }
    }

    println!("part 2: {sa}")
}

fn parse_voxels(input: &str) -> HashSet<[i32; 3]> {
    let mut grid = HashSet::new();
    for line in input.lines() {
        let nums: Vec<_> = line.split(",").map(|s| s.parse().unwrap()).collect();
        let x = nums.try_into().unwrap();
        grid.insert(x);
    }
    grid
}
