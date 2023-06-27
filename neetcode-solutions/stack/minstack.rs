struct MinStack {
    stack: Vec<i32>
}

impl MinStack {

    fn new() -> Self {
       MinStack { stack: Vec::new() } 
    }
    
    fn push(&mut self, val: i32) {
        self.stack.push(val)
        
    }
    
    fn pop(&mut self) {
        self.stack.pop();
    }
    
    fn top(&self) -> i32 {
        *self.stack.last().unwrap()
    }
    
    fn get_min(&self) -> i32 {
        *self.stack.iter().min().unwrap()
        
    }
}
