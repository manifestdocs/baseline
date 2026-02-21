package main

import "fmt"

const n = 200
const iterations = 50

func nextRand(seed int64) int64 {
	s := seed*1103515245 + 12345
	if s < 0 {
		s = -s
	}
	s = s % 1000000007
	return s
}

func merge(left, right []int64) []int64 {
	result := make([]int64, 0, len(left)+len(right))
	i, j := 0, 0
	for i < len(left) && j < len(right) {
		if left[i] <= right[j] {
			result = append(result, left[i])
			i++
		} else {
			result = append(result, right[j])
			j++
		}
	}
	result = append(result, left[i:]...)
	result = append(result, right[j:]...)
	return result
}

func mergesort(arr []int64) []int64 {
	if len(arr) <= 1 {
		sorted := make([]int64, len(arr))
		copy(sorted, arr)
		return sorted
	}
	mid := len(arr) / 2
	left := mergesort(arr[:mid])
	right := mergesort(arr[mid:])
	return merge(left, right)
}

func main() {
	original := make([]int64, n)
	seed := int64(42)
	for i := 0; i < n; i++ {
		seed = nextRand(seed)
		original[i] = seed % 10000
	}

	var firstElement int64
	for iter := 0; iter < iterations; iter++ {
		sorted := mergesort(original)
		firstElement = sorted[0]
	}

	fmt.Println(firstElement)
}
