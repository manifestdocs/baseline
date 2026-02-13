function divsum(n) {
    let total = 0;
    for (let i = 1; i <= n; i++) {
        let s = 0;
        for (let j = 1; j <= i; j++) {
            if (i % j === 0) s += j;
        }
        total += s;
    }
    return total;
}

console.log(divsum(10000));
