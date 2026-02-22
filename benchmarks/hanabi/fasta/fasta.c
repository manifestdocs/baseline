// Fasta benchmark — C reference implementation
// Based on The Computer Language Benchmarks Game
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define LINE_LENGTH 60
#define IM 139968
#define IA 3877
#define IC 29573

static int last = 42;

static int gen_random(void) {
    last = (last * IA + IC) % IM;
    return last;
}

struct AminoAcid { char c; double p; };

static struct AminoAcid iub[] = {
    {'a', 0.27}, {'c', 0.12}, {'g', 0.12}, {'t', 0.27},
    {'B', 0.02}, {'D', 0.02}, {'H', 0.02}, {'K', 0.02},
    {'M', 0.02}, {'N', 0.02}, {'R', 0.02}, {'S', 0.02},
    {'V', 0.02}, {'W', 0.02}, {'Y', 0.02},
};

static struct AminoAcid homosapiens[] = {
    {'a', 0.3029549426680},
    {'c', 0.1979883004921},
    {'g', 0.1975473066391},
    {'t', 0.3015094502008},
};

static void make_cumulative(struct AminoAcid *table, int n) {
    double cp = 0.0;
    for (int i = 0; i < n; i++) {
        cp += table[i].p;
        table[i].p = cp;
    }
}

static char select_random(struct AminoAcid *table, int n) {
    int r = gen_random();
    double rf = (double)r / IM;
    for (int i = 0; i < n; i++) {
        if (rf < table[i].p) return table[i].c;
    }
    return table[n - 1].c;
}

static void make_repeat_fasta(const char *header, const char *alu, int n) {
    printf("%s", header);
    int alu_len = strlen(alu);
    int pos = 0;
    char line[LINE_LENGTH + 2];
    while (n > 0) {
        int nb = n < LINE_LENGTH ? n : LINE_LENGTH;
        for (int i = 0; i < nb; i++) {
            line[i] = alu[pos];
            pos = (pos + 1) % alu_len;
        }
        line[nb] = '\n';
        line[nb + 1] = '\0';
        fputs(line, stdout);
        n -= nb;
    }
}

static void make_random_fasta(const char *header, struct AminoAcid *table, int table_n, int n) {
    printf("%s", header);
    char line[LINE_LENGTH + 2];
    while (n > 0) {
        int nb = n < LINE_LENGTH ? n : LINE_LENGTH;
        for (int i = 0; i < nb; i++) {
            line[i] = select_random(table, table_n);
        }
        line[nb] = '\n';
        line[nb + 1] = '\0';
        fputs(line, stdout);
        n -= nb;
    }
}

int main(int argc, char **argv) {
    int n = argc > 1 ? atoi(argv[1]) : 1000;
    const char *alu =
        "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG"
        "GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA"
        "CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT"
        "ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA"
        "GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG"
        "AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC"
        "AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA";

    make_cumulative(iub, 15);
    make_cumulative(homosapiens, 4);

    make_repeat_fasta(">ONE Homo sapiens alu\n", alu, n * 2);
    make_random_fasta(">TWO IUB ambiguity codes\n", iub, 15, n * 3);
    make_random_fasta(">THREE Homo sapiens frequency\n", homosapiens, 4, n * 5);

    return 0;
}
