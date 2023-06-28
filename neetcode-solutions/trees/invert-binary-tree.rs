use std::cell::RefCell;
use std::rc::Rc;
impl Solution {
    pub fn invert_tree(root: Option<Rc<RefCell<TreeNode>>>) -> Option<Rc<RefCell<TreeNode>>> {
        let binding = root?;
        let root = binding.as_ref().borrow();
        Some(Rc::new(RefCell::new(TreeNode {
            val: root.val,
            left: Solution::invert_tree(root.right.clone()),
            right: Solution::invert_tree(root.left.clone()),
        })))
    }
}
