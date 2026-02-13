fn divsum(n: i64) -> i64 {
    let mut total: i64 = 0;
    for i in 1..=n {
        let mut s: i64 = 0;
        for j in 1..=i {
            if i % j == 0 { s += j; }
        }
        total += s;
    }
    total
}

fn main() {
    println!("{}", divsum(10000));
}
