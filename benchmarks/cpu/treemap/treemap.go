package main

import "fmt"

// Binary search tree: insert 100000 keys, sum all values
// Tests: allocation pressure, pointer-based data structure, recursion

type Node struct {
	key, value int64
	left, right *Node
}

func insert(t *Node, key, val int64) *Node {
	if t == nil {
		return &Node{key: key, value: val}
	}
	if key < t.key {
		t.left = insert(t.left, key, val)
	} else if key > t.key {
		t.right = insert(t.right, key, val)
	} else {
		t.value = val
	}
	return t
}

func treeSum(t *Node) int64 {
	if t == nil {
		return 0
	}
	return t.value + treeSum(t.left) + treeSum(t.right)
}

func main() {
	var root *Node
	var seed int64 = 42
	for i := 0; i < 100000; i++ {
		seed = seed*1103515245 + 12345
		if seed < 0 {
			seed = -seed
		}
		seed = seed % 1000000007
		key := seed % 100000
		root = insert(root, key, key)
	}
	fmt.Println(treeSum(root))
}
