impl Solution {
    pub fn generate_parenthesis(n: i32) -> Vec<String> {
        // First usize corresponds to the total nesting
        let mut parens: Vec<(usize, String)> = vec![(0, String::from(""))];
        for i in (1..=(n * 2)).rev() {
            parens = parens
                .iter()
                .flat_map(|(nesting, str)| {
                    if *nesting as i32 == i {
                        vec![(nesting - 1, format!("{str})"))]
                    } else if *nesting == 0 {
                        vec![(nesting + 1, format!("{str}("))]
                    } else {
                        vec![
                            (nesting - 1, format!("{str})")),
                            (nesting + 1, format!("{str}(")),
                        ]
                    }
                })
                .collect();
        }
        parens.into_iter().map(|x| x.1).collect()
    }
}
