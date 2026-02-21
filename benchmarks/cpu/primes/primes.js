function isPrime(n) {
    if (n < 2) return false;
    let d = 2;
    while (d * d <= n) {
        if (n % d === 0) return false;
        d += 1;
    }
    return true;
}

let count = 0;
for (let n = 2; n <= 200000; n++) {
    if (isPrime(n)) count += 1;
}
console.log(count);
