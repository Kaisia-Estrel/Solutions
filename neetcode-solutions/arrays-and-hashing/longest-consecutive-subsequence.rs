impl Solution {
    pub fn longest_consecutive(nums: Vec<i32>) -> i32 {
        use std::collections::HashSet;
        let nums: HashSet<&i32> = nums.iter().collect();
        nums.iter()
            .map(|x| {
                if !nums.contains(&(*x - 1)) {
                    (**x..).take_while(|x| nums.contains(x)).count()
                } else {
                    1
                }
            })
            .max()
            .unwrap_or(0) as i32
    }
}
