// Binary search tree: insert 100000 keys, sum all values
// Tests: allocation pressure, object-based data structure, recursion

function insert(t, key, val) {
  if (t === null) return { key, value: val, left: null, right: null };
  if (key < t.key) t.left = insert(t.left, key, val);
  else if (key > t.key) t.right = insert(t.right, key, val);
  else t.value = val;
  return t;
}

function treeSum(t) {
  if (t === null) return 0;
  return t.value + treeSum(t.left) + treeSum(t.right);
}

let root = null;
let seed = 42n;
for (let i = 0; i < 100000; i++) {
  seed = seed * 1103515245n + 12345n;
  if (seed < 0n) seed = -seed;
  seed = seed % 1000000007n;
  const key = Number(seed % 100000n);
  root = insert(root, key, key);
}
console.log(treeSum(root));
