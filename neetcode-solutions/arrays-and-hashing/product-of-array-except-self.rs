impl Solution {
    pub fn product_except_self(nums: Vec<i32>) -> Vec<i32> {
        use std::iter::once;
        let prefix = once(1).chain(nums.iter().scan(1, |accum, i| {
            *accum *= i;
            Some(*accum)
        }));
        let postfix = once(1)
            .chain(nums.iter().rev().scan(1, |accum, i| {
                *accum *= i;
                Some(*accum)
            }))
            .collect::<Vec<_>>()
            .into_iter()
            .rev();
        prefix.zip(postfix.skip(1)).map(|(x, y)| x * y).collect()
    }
}
