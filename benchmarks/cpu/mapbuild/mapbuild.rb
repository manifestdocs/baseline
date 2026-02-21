# Build a Hash by inserting 20000 key-value pairs, then sum all values
# Tests: hash map, string key creation, allocation

m = {}
seed = 42
20000.times do
  seed = seed * 1103515245 + 12345
  seed = -seed if seed < 0
  seed = seed % 1000000007
  key = "key_#{seed % 5000}"
  m[key] = seed % 10000
end

total = m.values.sum
puts total
