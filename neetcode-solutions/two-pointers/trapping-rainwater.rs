struct ScanL<St, F, I>
where
    I: Iterator,
{
    f: F,
    st: Option<St>,
    iter: I,
}

impl<St, F, I> Iterator for ScanL<St, F, I>
where
    I: Iterator,
    F: Fn(&St, I::Item) -> St,
{
    type Item = St;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(x) = self.iter.next() {
            let new_state = (self.f)(self.st.as_ref()?, x);
            self.st.replace(new_state)
        } else {
            self.st.take()
        }
    }
}

trait Scan: Iterator {
    fn scanl<St, F>(self, init: St, f: F) -> ScanL<St, F, Self>
    where
        Self: Sized,
        F: Fn(&St, Self::Item) -> St,
    {
        ScanL {
            f,
            st: Some(init),
            iter: self,
        }
    }

    fn scanr<St, F>(self, init: St, f: F) -> std::vec::IntoIter<St>
    where
        Self: Sized,
        F: Fn(Self::Item, &St) -> St,
    {
        let items = self.collect::<Vec<_>>();
        let items_len = items.len();
        let mut out = Vec::with_capacity(items.len() + 1);
        unsafe {
            let base: *mut St = out.as_mut_ptr();
            base.add(items_len).write(init);

            for (i, x) in items.into_iter().enumerate().rev() {
                base.add(i)
                    .write(f(x, base.add(i + 1).as_ref().expect("should not occur")))
            }
            out.set_len(items_len + 1);
        }
        out.into_iter()
    }
}

impl<I: Iterator> Scan for I {}

impl Solution {
    pub fn trap(heights: Vec<i32>) -> i32 {
        let prefs = heights[1..]
            .iter()
            .scanl(heights[0], |x, y| std::cmp::max(*x, *y));
        let suffs = heights[2..heights.len() - 1]
            .iter()
            .scanr(heights[heights.len() - 1], |x, y| std::cmp::max(*x, *y));
        let mut ans = 0;
        for ((pref, suff), height) in prefs.zip(suffs).zip(heights[1..].iter()) {
            ans += std::cmp::max(0, std::cmp::min(pref, suff) - height);
        }
        ans
    }
}
