const N = 200;
const ITERATIONS = 50;

function nextRand(seed) {
    let s = seed * 1103515245 + 12345;
    if (s < 0) s = -s;
    s = s % 1000000007;
    return s;
}

function merge(left, right) {
    const result = [];
    let i = 0;
    let j = 0;
    while (i < left.length && j < right.length) {
        if (left[i] <= right[j]) {
            result.push(left[i]);
            i++;
        } else {
            result.push(right[j]);
            j++;
        }
    }
    while (i < left.length) {
        result.push(left[i]);
        i++;
    }
    while (j < right.length) {
        result.push(right[j]);
        j++;
    }
    return result;
}

function mergesort(arr) {
    if (arr.length <= 1) return arr.slice();
    const mid = Math.floor(arr.length / 2);
    const left = mergesort(arr.slice(0, mid));
    const right = mergesort(arr.slice(mid));
    return merge(left, right);
}

// Use BigInt for the PRNG to avoid floating-point precision issues
function nextRandBig(seed) {
    let s = seed * 1103515245n + 12345n;
    if (s < 0n) s = -s;
    s = s % 1000000007n;
    return s;
}

function main() {
    const original = [];
    let seed = 42n;
    for (let i = 0; i < N; i++) {
        seed = nextRandBig(seed);
        original.push(Number(seed % 10000n));
    }

    let firstElement = 0;
    for (let iter = 0; iter < ITERATIONS; iter++) {
        const sorted = mergesort(original);
        firstElement = sorted[0];
    }

    console.log(firstElement);
}

main();
