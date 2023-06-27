impl Solution {
    pub fn is_valid(s: String) -> bool {
        let mut stack = Vec::new();
        for c in s.chars() {
            match c {
                '(' | '[' | '{' => stack.push(c),
                ')' => {
                    if !stack.is_empty() && *stack.last().unwrap() == '(' {
                        stack.pop();
                    } else {
                        return false;
                    }
                }
                ']' => {
                    if !stack.is_empty() && *stack.last().unwrap() == '[' {
                        stack.pop();
                    } else {
                        return false;
                    }
                }
                '}' => {
                    if !stack.is_empty() && *stack.last().unwrap() == '{' {
                        stack.pop();
                    } else {
                        return false;
                    }
                }
                _ => todo!(),
            }
        }
        stack.is_empty()
    }
}
