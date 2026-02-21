#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define N 200
#define ITERATIONS 50

long long next_rand(long long seed) {
    long long s = seed * 1103515245LL + 12345LL;
    if (s < 0) s = -s;
    s = s % 1000000007LL;
    return s;
}

void merge(long long *arr, int left, int mid, int right) {
    int n1 = mid - left;
    int n2 = right - mid;

    long long *L = (long long *)malloc(n1 * sizeof(long long));
    long long *R = (long long *)malloc(n2 * sizeof(long long));

    for (int i = 0; i < n1; i++)
        L[i] = arr[left + i];
    for (int i = 0; i < n2; i++)
        R[i] = arr[mid + i];

    int i = 0, j = 0, k = left;
    while (i < n1 && j < n2) {
        if (L[i] <= R[j]) {
            arr[k++] = L[i++];
        } else {
            arr[k++] = R[j++];
        }
    }
    while (i < n1) arr[k++] = L[i++];
    while (j < n2) arr[k++] = R[j++];

    free(L);
    free(R);
}

void mergesort(long long *arr, int left, int right) {
    if (right - left <= 1) return;
    int mid = left + (right - left) / 2;
    mergesort(arr, left, mid);
    mergesort(arr, mid, right);
    merge(arr, left, mid, right);
}

int main(void) {
    long long original[N];
    long long arr[N];

    /* Generate 200 pseudo-random values */
    long long seed = 42;
    for (int i = 0; i < N; i++) {
        seed = next_rand(seed);
        original[i] = seed % 10000;
    }

    /* Sort 50 times, copying the original array each time */
    for (int iter = 0; iter < ITERATIONS; iter++) {
        memcpy(arr, original, N * sizeof(long long));
        mergesort(arr, 0, N);
    }

    printf("%lld\n", arr[0]);
    return 0;
}
