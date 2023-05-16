impl Solution {
    pub fn is_anagram(s: String, t: String) -> bool {
        if s.len() != t.len() {
            return false;
        }
        let mut s = s.chars().collect::<Vec<_>>();
        let mut t = t.chars().collect::<Vec<_>>();
        s.sort_unstable();
        t.sort_unstable();
        s.iter().zip(t.iter()).all(|(x,y)| x==y)
    }
}
