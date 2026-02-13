#include <stdio.h>

long divsum(int n) {
    long total = 0;
    for (int i = 1; i <= n; i++) {
        long s = 0;
        for (int j = 1; j <= i; j++) {
            if (i % j == 0) s += j;
        }
        total += s;
    }
    return total;
}

int main(void) {
    printf("%ld\n", divsum(10000));
    return 0;
}
