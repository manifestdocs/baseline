package main

import "fmt"

func divsum(n int) int64 {
	var total int64
	for i := 1; i <= n; i++ {
		var s int64
		for j := 1; j <= i; j++ {
			if i%j == 0 {
				s += int64(j)
			}
		}
		total += s
	}
	return total
}

func main() {
	fmt.Println(divsum(10000))
}
