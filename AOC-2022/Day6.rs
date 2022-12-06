fn has_dups(xs: &[u8]) -> bool {
    let mut uniq = std::collections::HashSet::new();
    !xs.iter().all(move |x| uniq.insert(x))
}
fn main() {
    println!("{} {}", 
        std::fs::read("input.txt").unwrap().windows(4).take_while(|x|has_dups(x)).count() + 4
    ,   std::fs::read("input.txt").unwrap().windows(14).take_while(|x|has_dups(x)).count() + 14
    );
}
