trait DuplicatesTrait {
    fn duplicates(self) -> bool
    where
        Self: std::iter::IntoIterator + std::marker::Sized,
        <Self as std::iter::IntoIterator>::Item: std::hash::Hash,
        <Self as std::iter::IntoIterator>::Item: std::cmp::Eq,
    {
        use std::collections::HashSet;
        let mut set = HashSet::new();
        for i in self {
            if set.get(&i).is_some() {
                return true;
            }
            set.insert(i);
        }
        false
    }
}

impl<'a, T> DuplicatesTrait for std::slice::Iter<'a, T> {}
impl<T, P> DuplicatesTrait for std::iter::Map<T, P> {}
impl<T, P> DuplicatesTrait for std::iter::Filter<T, P> {}

impl Solution {
    pub fn is_valid_sudoku(board: Vec<Vec<char>>) -> bool {
        dbg!(vec![1, 2, 3, 4].iter().duplicates());
        dbg!(vec![1, 2, 2, 4].iter().duplicates());
        // Check rows
        if board
            .iter()
            .any(|x| x.iter().filter(|&&x| x != '.').duplicates())
        {
            return false;
        }

        // Check collumns via transposed rows
        for i in 0..9 {
            if board[0..9]
                .iter()
                .map(|x| x[i])
                .filter(|&x| x != '.')
                .duplicates()
            {
                return false;
            }
        }

        for x in 0..3 {
            for y in 0..3 {
                let mut sudoko_box = Vec::new();
                for i in 0..3 {
                    for j in 0..3 {
                        sudoko_box.push(board[x * 3 + i][y * 3 + j]);
                    }
                }
                if sudoko_box.iter().filter(|&&x|x!='.').duplicates() {
                    return false;
                }
            }
        }
        true
    }
}
