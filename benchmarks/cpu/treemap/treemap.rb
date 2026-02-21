# Binary search tree: insert 100000 keys, sum all values
# Tests: allocation pressure, class-based data structure, recursion

class Node
  attr_accessor :key, :value, :left, :right
  def initialize(key, value)
    @key = key
    @value = value
    @left = nil
    @right = nil
  end
end

def insert(t, key, val)
  return Node.new(key, val) if t.nil?
  if key < t.key
    t.left = insert(t.left, key, val)
  elsif key > t.key
    t.right = insert(t.right, key, val)
  else
    t.value = val
  end
  t
end

def tree_sum(t)
  return 0 if t.nil?
  t.value + tree_sum(t.left) + tree_sum(t.right)
end

root = nil
seed = 42
100000.times do
  seed = seed * 1103515245 + 12345
  seed = -seed if seed < 0
  seed = seed % 1000000007
  key = seed % 100000
  root = insert(root, key, key)
end
puts tree_sum(root)
