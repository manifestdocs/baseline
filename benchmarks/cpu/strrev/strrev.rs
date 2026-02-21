// Build a string of N digits, reverse it 200 times, print length
// Tests: string allocation, character-level operations

fn build_string(n: usize) -> String {
    let mut s = String::with_capacity(n);
    for i in 0..n {
        s.push((b'0' + (i % 10) as u8) as char);
    }
    s
}

fn reverse_string(s: &str) -> String {
    s.chars().rev().collect()
}

fn main() {
    let n = 1000;
    let mut s = build_string(n);
    for _ in 0..200 {
        s = reverse_string(&s);
    }
    println!("{}", s.len());
}
