use std::cmp::*;

struct Solution {}

impl Solution {
    pub fn max_area(height: Vec<i32>) -> i32 {
        let mut max_area_result = 0;
        let mut l: usize = 0;
        let mut r: usize = height.len() - 1;

        while l < r {
            let area = (r - l) as i32 * min(height[l], height[r]);
            max_area_result = max(max_area_result, area);
            if height[l] < height[r] {
                l += 1;
            } else {
                r -= 1;
            }
        }
        max_area_result
    }
}

fn main() {
    dbg!(Solution::max_area(vec![1, 8, 6, 2, 5, 4, 8, 3, 7]));
}
