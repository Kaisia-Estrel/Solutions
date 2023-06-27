impl Solution {
    pub fn group_anagrams(strs: Vec<String>) -> Vec<Vec<String>> {
        use std::collections::HashMap;
        let mut map: HashMap<Vec<char>, Vec<String>> = HashMap::new();
        for i in strs.into_iter() {
            let mut sorted: Vec<_> = i.chars().collect();
            sorted.sort();
            match map.get_mut(&sorted) {
                None => {
                    map.insert(sorted, vec![i]);
                }
                Some(vec) => {
                    vec.push(i);
                }
            }
        }
        map.into_values().collect()
    }
}
