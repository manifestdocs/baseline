#include <stdio.h>
#include <stdbool.h>

bool is_prime(int n) {
    if (n < 2) return false;
    int d = 2;
    while (d * d <= n) {
        if (n % d == 0) return false;
        d += 1;
    }
    return true;
}

int main(void) {
    int count = 0;
    for (int n = 2; n <= 200000; n++) {
        if (is_prime(n)) count += 1;
    }
    printf("%d\n", count);
    return 0;
}
