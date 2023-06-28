use std::cell::RefCell;
use std::rc::Rc;

struct InorderIter {
    stack: Vec<Rc<RefCell<TreeNode>>>,
    tree: Option<Rc<RefCell<TreeNode>>>,
}

impl Iterator for InorderIter {
    type Item = i32;

    fn next(&mut self) -> Option<Self::Item> {
        let tree = &mut self.tree;
        let stack = &mut self.stack;
        let mut out = None;
        if tree.is_some() || !stack.is_empty() {
            while let Some(inner) = tree {
                stack.push(Rc::clone(inner));
                *tree = Rc::clone(inner).as_ref().borrow().left.clone();
            }

            if let Some(top) = stack.pop() {
                *tree = Some(Rc::clone(&top));
                out = Some(top.as_ref().borrow().val);
            }

            if let Some(inner) = &tree {
                *tree = Rc::clone(inner).as_ref().borrow().right.clone();
            }
        }
        out
    }
}

trait InorderTraversal {
    fn in_order_iter(self) -> InorderIter;
}

impl InorderTraversal for Rc<RefCell<TreeNode>> {
    fn in_order_iter(self) -> InorderIter {
        InorderIter {
            stack: Vec::new(),
            tree: Some(self)
        }
    }
}

impl Solution {
    pub fn kth_smallest(root: Option<Rc<RefCell<TreeNode>>>, k: i32) -> i32 {
        root.unwrap().in_order_iter().nth(k as usize - 1).unwrap()
    }
}
