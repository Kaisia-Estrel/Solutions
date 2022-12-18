use std::collections::{HashMap, HashSet, VecDeque};

use nom::branch::alt;
use nom::bytes::complete::{tag, take_while1};
use nom::character::complete::newline;
use nom::multi::separated_list1;
use nom::IResult;

use petgraph::graphmap::UnGraphMap;

fn valve(s: &str) -> IResult<&str, (&str, usize, Vec<&str>)> {
    let (s, _) = tag("Valve ")(s)?;
    let (s, name) = take_while1(|x: char| x.is_uppercase())(s)?;
    let (s, _) = tag(" has flow rate=")(s)?;
    let (s, flow) = take_while1(|x: char| x.is_digit(10))(s)?;
    let (s, _) = alt((
        tag("; tunnels lead to valves "),
        tag("; tunnel leads to valve "),
    ))(s)?;
    let (s, connections) = separated_list1(tag(", "), take_while1(|x: char| x.is_uppercase()))(s)?;
    Ok((s, (name, flow.parse().unwrap(), connections)))
}

fn valves(input: &str) -> IResult<&str, HashMap<&str, (usize, Vec<&str>)>> {
    let (input, out) = separated_list1(newline, valve)(input)?;
    let mut map = HashMap::new();
    for (name, flow, neighbors) in out.into_iter() {
        map.insert(name, (flow, neighbors));
    }
    Ok((input, map))
}

fn distances<'a>(current: &'a str, graph: &UnGraphMap<&'a str, usize>) -> Vec<(&'a str, usize)> {
    let mut out = vec![(current, 0)];
    let mut visited: HashSet<&'a str> = [current].into_iter().collect();
    let mut queue: VecDeque<(&str, usize)> = graph.neighbors(current).map(|x| (x, 1)).collect();
    while let Some((x, level)) = queue.pop_front() {
        if visited.insert(x) {
            out.push((x, level));
            graph
                .neighbors(x)
                .for_each(|i| queue.push_back((i, level + 1)));
        }
    }
    out
}

fn releases<'a>(
    current: &'a str,
    graph: &UnGraphMap<&'a str, usize>,
    map: &HashMap<&'a str, (usize, Vec<&str>)>,
    remaining: usize,
) -> Vec<(&'a str, usize, usize)> {
    distances(current, graph)
        .into_iter()
        .filter_map(move |(name, distance)| {
            let total_flow = Some(map[name].0 * (remaining.checked_sub(distance+1)?));
            Some((name, total_flow.filter(|x| *x>0)?, distance+1))
        }).collect()
}

#[derive(Debug)]
struct DecisionTree<'a> {
    name: &'a str,
    total_pressure: usize,
    roots: Vec<DecisionTree<'a>>,
}

impl<'a> DecisionTree<'a> {
    fn new(name: &'a str, total_pressure: usize, roots: Vec<DecisionTree<'a>>) -> Self {
        Self {name,total_pressure,roots}
    }
    fn decisions(&self) -> Vec<(Vec<&str>, usize)> {
        self.roots.iter().flat_map(|x| decisions(vec![], 0, x)).collect()
    }
}

fn decisions<'a>(
    mut path: Vec<&'a str>,
    total_pressure: usize,
    tree: &DecisionTree<'a>,
) -> Vec<(Vec<&'a str>, usize)> {
    if tree.roots.is_empty() {
        path.push(tree.name);
        return vec![(path, total_pressure + tree.total_pressure)];
    }
    let mut out = Vec::new();
    for i in tree.roots.iter() {
        let mut path = path.clone();
        path.push(tree.name);
        out.append(&mut decisions(
            path,
            total_pressure + tree.total_pressure,
            i,
        ));
    }
    out
}

fn decision_tree<'a>(
    current: &'a str,
    graph: &UnGraphMap<&'a str, usize>,
    visited: HashSet<&'a str>,
    map: &HashMap<&'a str, (usize, Vec<&str>)>,
    remaining: usize,
) -> DecisionTree<'a> {
    let mut roots = Vec::new();
    for (name, _flow, skip) in releases(current, graph, map, remaining) {
        if visited.contains(name) { continue; }
        let mut visited = visited.clone();
        visited.insert(name);
        roots.push(decision_tree(name, graph, visited, map, remaining-skip))
    }
    DecisionTree::new(current, map[current].0 * remaining, roots)
}

fn main() {
    let file = std::fs::read_to_string("input.txt").unwrap();

    let map = valves(&file).unwrap().1;
    let mut graph = UnGraphMap::<&str, usize>::new();
    for (k, (_, neighbors)) in map.iter() {
        for i in neighbors.iter() {
            graph.add_edge(*k, *i, 1);
        }
    }

    let tree = decision_tree("AA", &graph, HashSet::new(), &map, 30);
    println!("{:?}",tree.decisions().iter().max_by_key(|x| x.1));
}
