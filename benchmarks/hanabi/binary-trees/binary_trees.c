// Binary Trees benchmark — C reference implementation
#include <stdio.h>
#include <stdlib.h>

typedef struct Node {
    struct Node *left, *right;
} Node;

static Node *make(int depth) {
    Node *n = malloc(sizeof(Node));
    if (depth <= 0) {
        n->left = n->right = NULL;
    } else {
        n->left = make(depth - 1);
        n->right = make(depth - 1);
    }
    return n;
}

static int check(Node *n) {
    if (!n->left) return 1;
    return 1 + check(n->left) + check(n->right);
}

static void free_tree(Node *n) {
    if (n->left) { free_tree(n->left); free_tree(n->right); }
    free(n);
}

int main(int argc, char **argv) {
    int n = argc > 1 ? atoi(argv[1]) : 21;
    int min_depth = 4;
    int max_depth = (min_depth + 2 > n) ? min_depth + 2 : n;
    int stretch_depth = max_depth + 1;

    Node *stretch = make(stretch_depth);
    printf("stretch tree of depth %d\t check: %d\n", stretch_depth, check(stretch));
    free_tree(stretch);

    Node *long_lived = make(max_depth);

    for (int depth = min_depth; depth <= max_depth; depth += 2) {
        int iterations = 1;
        for (int i = 0; i < max_depth - depth + min_depth; i++) iterations *= 2;
        int c = 0;
        for (int i = 0; i < iterations; i++) {
            Node *t = make(depth);
            c += check(t);
            free_tree(t);
        }
        printf("%d\t trees of depth %d\t check: %d\n", iterations, depth, c);
    }

    printf("long lived tree of depth %d\t check: %d\n", max_depth, check(long_lived));
    free_tree(long_lived);
    return 0;
}
