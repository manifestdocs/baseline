# Binary search tree: insert 100000 keys, sum all values
# Tests: allocation pressure, class-based data structure, recursion
import sys
sys.setrecursionlimit(200000)

class Node:
    __slots__ = ('key', 'value', 'left', 'right')
    def __init__(self, key, value):
        self.key = key
        self.value = value
        self.left = None
        self.right = None

def insert(t, key, val):
    if t is None:
        return Node(key, val)
    if key < t.key:
        t.left = insert(t.left, key, val)
    elif key > t.key:
        t.right = insert(t.right, key, val)
    else:
        t.value = val
    return t

def tree_sum(t):
    if t is None:
        return 0
    return t.value + tree_sum(t.left) + tree_sum(t.right)

root = None
seed = 42
for _ in range(100000):
    seed = seed * 1103515245 + 12345
    if seed < 0:
        seed = -seed
    seed = seed % 1000000007
    key = seed % 100000
    root = insert(root, key, key)

print(tree_sum(root))
