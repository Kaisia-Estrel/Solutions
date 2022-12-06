use itertools::Itertools;

fn transpose(xs: &Vec<Vec<char>>) -> Vec<Vec<char>> {
    let mut out = vec![vec![' '; xs.len()]; xs[0].len()];
    for i in 0..xs.len() {
        for j in 0..xs[0].len() {
            out[j][i] = xs[i][j];
        }
    }
    out
}
fn mult_pop(xs: &mut Vec<char>, n: usize) -> Option<Vec<char>> {
    let mut out = Vec::with_capacity(n);
    for _ in 0..n {
        out.push(xs.pop())
    }
    out.into_iter().collect()
}

fn main() {
    let file = std::fs::read_to_string("input.txt").unwrap();
    let lines = file.lines().collect_vec();
    let (l, r) = lines.split_at(8);
    let grid_ = transpose(
        &l.iter()
            .map(|&x| {
                x.chars()
                    .skip(1)
                    .enumerate()
                    .filter_map(|(i, c)| if 0 == i % 4 { Some(c) } else { None })
                    .collect_vec()
            })
            .collect_vec(),
    );

    let mut grid = grid_
        .iter()
        .map(|x| x.iter().filter_map(|x| if !x.is_whitespace() { Some(*x)} else {None}).rev().collect_vec())
        .collect_vec();
    let mut grid_clone = grid.clone();

    let instructions = r
        .iter()
        .map(|i| {
            i.split_whitespace()
                .filter_map(|x| x.parse::<usize>().map_or(None, Some))
                .collect_tuple::<(usize, usize, usize)>()
        })
        .filter_map(|x| x);
    for (quant, from, to) in instructions.clone() {
        for i in mult_pop(&mut grid[from-1], quant).unwrap().iter() {
            grid[to-1].push(*i);
        }
    }
    println!("part1: {:?}", grid.iter().map(|x|x.last().unwrap()).collect::<String>());
    for (quant, from, to) in instructions.clone() {
        for i in mult_pop(&mut grid_clone[from-1], quant).unwrap().iter().rev() {
            grid_clone[to-1].push(*i);
        }
    }
    println!("part2: {:?}", grid_clone.iter().map(|x|x.last().unwrap()).collect::<String>());
}
