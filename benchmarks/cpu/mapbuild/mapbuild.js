// Build a Map by inserting 20000 key-value pairs, then sum all values
// Tests: hash map, string key creation, allocation

const map = new Map();
let seed = 42n;
for (let i = 0; i < 20000; i++) {
  seed = seed * 1103515245n + 12345n;
  if (seed < 0n) seed = -seed;
  seed = seed % 1000000007n;
  const key = `key_${seed % 5000n}`;
  map.set(key, Number(seed % 10000n));
}
let total = 0;
for (const v of map.values()) {
  total += v;
}
console.log(total);
