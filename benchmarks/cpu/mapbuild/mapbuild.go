package main

import "fmt"

// Build a map by inserting 20000 key-value pairs, then sum all values
// Tests: hash map, string key creation, allocation

func main() {
	m := make(map[string]int64)
	var seed int64 = 42
	for i := 0; i < 20000; i++ {
		seed = seed*1103515245 + 12345
		if seed < 0 {
			seed = -seed
		}
		seed = seed % 1000000007
		key := fmt.Sprintf("key_%d", seed%5000)
		m[key] = seed % 10000
	}
	var total int64
	for _, v := range m {
		total += v
	}
	fmt.Println(total)
}
