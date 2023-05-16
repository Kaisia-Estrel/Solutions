impl Solution {
    pub fn contains_duplicate(nums: Vec<i32>) -> bool {
        use std::collections::HashSet;
        let mut set = HashSet::new();
        for i in nums.iter() {
            if !set.insert(i) {
                return true;
            }
        }
        false
    }
}
