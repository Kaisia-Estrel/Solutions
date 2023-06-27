impl Solution {
    pub fn top_k_frequent(nums: Vec<i32>, k: i32) -> Vec<i32> {
        use std::collections::HashMap;
        let mut map = HashMap::new();
        for i in nums.into_iter() {
            if let Some(x) = map.get_mut(&i) {
                *x += 1;
            } else {
                map.insert(i, 0);
            }
        }
        let mut map: Vec<_> = map.into_iter().collect();
        map.sort_by_key(|x| std::cmp::Reverse(x.1) );
        map.iter().map(|x|x.0).take(k as usize).collect()
    }
}
