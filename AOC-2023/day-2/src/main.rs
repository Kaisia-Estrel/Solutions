#![feature(let_chains)]

type PResult<'a, T> = Option<(T, &'a str)>;

fn cond(p: impl Fn(char) -> bool) -> impl Fn(&str) -> PResult<char> {
    move |str| {
        if let Some(c) = str.chars().next()
            && p(c)
        {
            Some((c, &str[1..]))
        } else {
            None
        }
    }
}

fn string<'a>(prefix: &'a str) -> impl Fn(&'a str) -> PResult<&'a str> {
    move |str| str.strip_prefix(prefix).map(|s| (prefix, s))
}

fn many<'a, T>(
    parser: impl Fn(&'a str) -> PResult<'a, T>,
) -> impl Fn(&'a str) -> PResult<'a, Vec<T>> {
    move |mut str| {
        let mut results = Vec::new();
        while let Some((x, s)) = parser(str) {
            results.push(x);
            str = s;
        }
        Some((results, str))
    }
}

fn many1<T>(parser: impl Fn(&str) -> PResult<T>) -> impl Fn(&str) -> PResult<Vec<T>> {
    move |str| {
        let (x, str) = parser(str)?;
        let (mut xs, str) = many(&parser)(str)?;
        let mut results = vec![x];
        results.append(&mut xs);
        Some((results, str))
    }
}

fn whitespace(str: &str) -> PResult<()> {
    Some(((), str.trim_start()))
}

fn seq_l<'a, A, B>(
    parser_a: impl Fn(&'a str) -> PResult<'a, A>,
    parser_b: impl Fn(&'a str) -> PResult<'a, B>,
) -> impl Fn(&'a str) -> PResult<'a, A> {
    move |str| {
        let (x, str) = parser_a(str)?;
        let (_, str) = parser_b(str)?;
        Some((x, str))
    }
}

fn lexeme<'a, T>(parser: impl Fn(&'a str) -> PResult<'a, T>) -> impl Fn(&'a str) -> PResult<'a, T> {
    seq_l(parser, whitespace)
}

fn symbol<'a>(sym: &'a str) -> impl Fn(&'a str) -> PResult<&'a str> {
    move |str| {
        let (s, str) = string(sym)(str)?;
        let (_, str) = whitespace(str)?;
        Some((s, str))
    }
}

fn usize(str: &str) -> PResult<usize> {
    let (digits, str) = many1(cond(|c| c.is_ascii_digit()))(str)?;
    if let Ok(num) = digits.iter().collect::<String>().parse() {
        Some((num, str))
    } else {
        None
    }
}

fn choice<'a, T>(
    parser_1: impl Fn(&'a str) -> PResult<'a, T>,
    parser_2: impl Fn(&'a str) -> PResult<'a, T>,
) -> impl Fn(&'a str) -> PResult<T> {
    move |str| {
        if let Some((x, str)) = parser_1(str) {
            Some((x, str))
        } else {
            parser_2(str)
        }
    }
}

fn choice3<'a, T>(
    parser_1: impl Fn(&'a str) -> PResult<'a, T>,
    parser_2: impl Fn(&'a str) -> PResult<'a, T>,
    parser_3: impl Fn(&'a str) -> PResult<'a, T>,
) -> impl Fn(&'a str) -> PResult<T> {
    choice(parser_1, choice(parser_2, parser_3))
}

fn sep_by1<'a, T, U>(
    parser: impl Fn(&'a str) -> PResult<'a, T>,
    separator: impl Fn(&'a str) -> PResult<'a, U>,
) -> impl Fn(&'a str) -> PResult<'a, Vec<T>> {
    move |str| {
        let (mut xs, str) = many(seq_l(&parser, &separator))(str)?;

        if let Some((x, str)) = parser(str) {
            xs.push(x);
            Some((xs, str))
        } else {
            None
        }
    }
}

#[derive(Debug)]
struct Set {
    red: usize,
    blue: usize,
    green: usize,
}

fn set(str: &str) -> PResult<Set> {
    fn item(str: &str) -> PResult<(usize, &str)> {
        let (count, str) = lexeme(usize)(str)?;
        let (color, str) = choice3(symbol("red"), symbol("green"), symbol("blue"))(str)?;
        Some(((count, color), str))
    }

    let (items, str) = sep_by1(lexeme(item), symbol(","))(str)?;
    let mut set = Set {red: 0, blue: 0, green: 0, };
    for (count, color) in items.iter() {
        match *color {
            "red" => set.red += count,
            "blue" => set.blue += count,
            "green" => set.green += count,
            _ => {}
        }
    }
    Some((set, str))
}

#[derive(Debug)]
struct Game {
    id: usize,
    sets: Vec<Set>,
}

fn game(str: &str) -> PResult<Game> {
    let (_, str) = symbol("Game")(str)?;
    let (id, str) = usize(str)?;
    let (_, str) = symbol(":")(str)?;
    let (sets, str) = sep_by1(set, symbol(";"))(str)?;
    Some((Game { id, sets }, str))
}

fn is_valid_set(Set { red, blue, green }: &Set) -> bool {
    *red <= 12 && *blue <= 14 && *green <= 13
}

fn main() {
    let binding = std::fs::read_to_string("./input.txt").unwrap();
    let part1_result = binding
        .lines()
        .map(|x| game(x).unwrap().0)
        .filter_map(|Game { id, sets }| {
            if sets.iter().all(is_valid_set) {
                Some(id)
            } else {
                None
            }
        })
        .sum::<usize>();
    dbg!(part1_result);

    let part2_result = binding
        .lines()
        .map(|x| {
            let Game { id: _, sets } = game(x).unwrap().0;
            let max_red = sets.iter().map(|x| x.red).max().unwrap();
            let max_greens = sets.iter().map(|x| x.green).max().unwrap();
            let max_blues = sets.iter().map(|x| x.blue).max().unwrap();
            max_red * max_blues * max_greens
        })
        .sum::<usize>();
    dbg!(part2_result);
}
