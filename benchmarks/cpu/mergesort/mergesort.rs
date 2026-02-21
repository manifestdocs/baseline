const N: usize = 200;
const ITERATIONS: usize = 50;

fn next_rand(seed: i64) -> i64 {
    let mut s = seed * 1103515245_i64 + 12345_i64;
    if s < 0 {
        s = -s;
    }
    s % 1000000007_i64
}

fn merge(left: &[i64], right: &[i64]) -> Vec<i64> {
    let mut result = Vec::with_capacity(left.len() + right.len());
    let mut i = 0;
    let mut j = 0;
    while i < left.len() && j < right.len() {
        if left[i] <= right[j] {
            result.push(left[i]);
            i += 1;
        } else {
            result.push(right[j]);
            j += 1;
        }
    }
    while i < left.len() {
        result.push(left[i]);
        i += 1;
    }
    while j < right.len() {
        result.push(right[j]);
        j += 1;
    }
    result
}

fn mergesort(arr: &[i64]) -> Vec<i64> {
    let len = arr.len();
    if len <= 1 {
        return arr.to_vec();
    }
    let mid = len / 2;
    let left = mergesort(&arr[..mid]);
    let right = mergesort(&arr[mid..]);
    merge(&left, &right)
}

fn main() {
    let mut original = Vec::with_capacity(N);
    let mut seed: i64 = 42;
    for _ in 0..N {
        seed = next_rand(seed);
        original.push(seed % 10000);
    }

    let mut first_element: i64 = 0;
    for _ in 0..ITERATIONS {
        let sorted = mergesort(&original);
        first_element = sorted[0];
    }

    println!("{}", first_element);
}
