package main

import "fmt"

func isPrime(n int) bool {
	if n < 2 {
		return false
	}
	d := 2
	for d*d <= n {
		if n%d == 0 {
			return false
		}
		d += 1
	}
	return true
}

func main() {
	count := 0
	for n := 2; n <= 200000; n++ {
		if isPrime(n) {
			count += 1
		}
	}
	fmt.Println(count)
}
