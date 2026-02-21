// Build a HashMap by inserting 20000 key-value pairs, then sum all values
// Tests: hash map, string key creation, allocation

use std::collections::HashMap;

fn main() {
    let mut map = HashMap::new();
    let mut seed: i64 = 42;
    for _ in 0..20000 {
        seed = seed.wrapping_mul(1103515245).wrapping_add(12345);
        if seed < 0 { seed = -seed; }
        seed %= 1000000007;
        let key = format!("key_{}", seed % 5000);
        map.insert(key, seed % 10000);
    }
    let total: i64 = map.values().sum();
    println!("{}", total);
}
