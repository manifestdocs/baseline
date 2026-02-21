#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Build a hash map by inserting 20000 key-value pairs, then sum all values
// Tests: hash map (open-addressing), string key creation, allocation

#define TABLE_SIZE 8192
#define MAX_KEY 32

typedef struct Entry {
    char key[MAX_KEY];
    long long value;
    struct Entry *next;
} Entry;

static Entry *table[TABLE_SIZE];

static unsigned int hash(const char *s) {
    unsigned int h = 5381;
    while (*s) {
        h = ((h << 5) + h) + (unsigned char)*s++;
    }
    return h % TABLE_SIZE;
}

static void insert(const char *key, long long value) {
    unsigned int h = hash(key);
    Entry *e = table[h];
    while (e) {
        if (strcmp(e->key, key) == 0) {
            e->value = value;
            return;
        }
        e = e->next;
    }
    Entry *n = malloc(sizeof(Entry));
    strncpy(n->key, key, MAX_KEY - 1);
    n->key[MAX_KEY - 1] = '\0';
    n->value = value;
    n->next = table[h];
    table[h] = n;
}

int main(void) {
    memset(table, 0, sizeof(table));

    long long seed = 42;
    for (int i = 0; i < 20000; i++) {
        seed = seed * 1103515245LL + 12345LL;
        if (seed < 0) seed = -seed;
        seed = seed % 1000000007LL;
        char key[MAX_KEY];
        snprintf(key, MAX_KEY, "key_%lld", seed % 5000);
        insert(key, seed % 10000);
    }

    long long total = 0;
    for (int i = 0; i < TABLE_SIZE; i++) {
        Entry *e = table[i];
        while (e) {
            total += e->value;
            Entry *next = e->next;
            free(e);
            e = next;
        }
    }
    printf("%lld\n", total);
    return 0;
}
