// Build a string of N digits, reverse it 200 times, print length
// Tests: string allocation, character-level operations

function buildString(n) {
  let s = '';
  for (let i = 0; i < n; i++) {
    s += String(i % 10);
  }
  return s;
}

function reverseString(s) {
  return s.split('').reverse().join('');
}

const n = 1000;
let s = buildString(n);
for (let i = 0; i < 200; i++) {
  s = reverseString(s);
}
console.log(s.length);
