package main

import (
	"fmt"
	"strings"
)

// Build a string of N digits, reverse it 200 times, print length
// Tests: string allocation, character-level operations

func buildString(n int) string {
	var sb strings.Builder
	sb.Grow(n)
	for i := 0; i < n; i++ {
		sb.WriteByte('0' + byte(i%10))
	}
	return sb.String()
}

func reverseString(s string) string {
	runes := []rune(s)
	for i, j := 0, len(runes)-1; i < j; i, j = i+1, j-1 {
		runes[i], runes[j] = runes[j], runes[i]
	}
	return string(runes)
}

func main() {
	n := 1000
	s := buildString(n)
	for i := 0; i < 200; i++ {
		s = reverseString(s)
	}
	fmt.Println(len(s))
}
