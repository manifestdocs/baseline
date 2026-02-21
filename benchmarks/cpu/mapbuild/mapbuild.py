# Build a dict by inserting 20000 key-value pairs, then sum all values
# Tests: hash map, string key creation, allocation

m = {}
seed = 42
for _ in range(20000):
    seed = seed * 1103515245 + 12345
    if seed < 0:
        seed = -seed
    seed = seed % 1000000007
    key = f"key_{seed % 5000}"
    m[key] = seed % 10000

total = sum(m.values())
print(total)
