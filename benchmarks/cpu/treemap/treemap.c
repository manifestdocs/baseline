#include <stdio.h>
#include <stdlib.h>

// Binary search tree: insert 100000 keys, sum all values
// Tests: allocation pressure, pointer-based data structure, recursion

typedef struct Node {
    int key, value;
    struct Node *left, *right;
} Node;

static Node *insert(Node *t, int key, int val) {
    if (!t) {
        Node *n = malloc(sizeof(Node));
        n->key = key; n->value = val;
        n->left = n->right = NULL;
        return n;
    }
    if (key < t->key) t->left = insert(t->left, key, val);
    else if (key > t->key) t->right = insert(t->right, key, val);
    else t->value = val;
    return t;
}

static long long tree_sum(Node *t) {
    if (!t) return 0;
    return t->value + tree_sum(t->left) + tree_sum(t->right);
}

static void tree_free(Node *t) {
    if (!t) return;
    tree_free(t->left);
    tree_free(t->right);
    free(t);
}

int main(void) {
    Node *root = NULL;
    long long seed = 42;
    for (int i = 0; i < 100000; i++) {
        seed = seed * 1103515245LL + 12345LL;
        if (seed < 0) seed = -seed;
        seed = seed % 1000000007LL;
        int key = (int)(seed % 100000);
        root = insert(root, key, key);
    }
    printf("%lld\n", tree_sum(root));
    tree_free(root);
    return 0;
}
