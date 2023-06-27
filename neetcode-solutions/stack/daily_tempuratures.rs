impl Solution {
    pub fn daily_temperatures(temperatures: Vec<i32>) -> Vec<i32> {
        let mut out = vec![0; temperatures.len()];
        let mut stack: Vec<usize> = Vec::new();
        for (i, t) in temperatures.iter().enumerate() {
            while !stack.is_empty() && *t > temperatures[*stack.last().unwrap()] as i32 {
                let stack_i = stack.pop().unwrap();
                out[stack_i] = i as i32 - stack_i as i32
            }
            stack.push(i)
        }
        out
    }
}
