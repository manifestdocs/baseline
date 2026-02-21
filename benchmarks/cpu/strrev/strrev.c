#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Build a string of N digits, reverse it 200 times, print length
// Tests: string allocation, character-level operations

static char *build_string(int n) {
    char *s = malloc(n + 1);
    for (int i = 0; i < n; i++) {
        s[i] = '0' + (i % 10);
    }
    s[n] = '\0';
    return s;
}

static char *reverse_string(const char *s, int len) {
    char *r = malloc(len + 1);
    for (int i = 0; i < len; i++) {
        r[i] = s[len - 1 - i];
    }
    r[len] = '\0';
    return r;
}

int main(void) {
    int n = 1000;
    char *s = build_string(n);
    int len = n;
    for (int i = 0; i < 200; i++) {
        char *rev = reverse_string(s, len);
        free(s);
        s = rev;
    }
    printf("%d\n", len);
    free(s);
    return 0;
}
