#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_ARRAY 1000000

int readStandardData(int *, int *);

int countDist(int *, int, int);

int countBillboards(int *, int, int);

int compare(int *, int *);

int main(void) {
    int bill_cnt, positions[MAX_ARRAY], dist = 0, n = MAX_ARRAY;

    if (!readStandardData(positions, &n)) {
        printf("Nespravny vstup.\n");
        return 1;
    }

    // qsort function from stdlib.h for sort int array by values from min to max
    qsort(positions, n, sizeof(*positions), (int (*)(const void *, const void *)) compare);

    printf("Pocet billboardu:\n");

    while (scanf("%d", &bill_cnt) == 1 && bill_cnt >= 0) {
        dist = countDist(positions, bill_cnt, n);

        if (dist == -1)
            printf("Vzdalenost: inf\n");
        else if (dist == -2)
            printf("N/A\n");
        else
            printf("Vzdalenost: %d\n", dist);
    }

    if (!feof(stdin)) {
        printf("Nespravny vstup.\n");
        return 1;
    }

    return 0;
}

int readStandardData(int positions[], int *n) {
    char a;
    int i = 0;

    printf("Mozna umisteni:\n");

    if (scanf(" %c", &a) == 1 && a == '{') {
        while (scanf("%d %c", &positions[i], &a) == 2 && i < *n && positions[i] >= 0) {
            if (a == ',')
                i++;
            else if (a == '}') {
                i++;
                *n = i;
                return 1;
            } else
                return 0;
        }
    }
    return 0;
}

int countDist(int positions[], int cnt, int n) {
    int i, x, max_iterations, found = 0;

    if (cnt == 0 || cnt == 1)
        return -1;
    else if (cnt > n)
        return -2;

    max_iterations = positions[n - 1] + 1;

    for (i = 0; i < max_iterations; i++) {
        x = countBillboards(positions, i, n);

        if (x == cnt) { found = 1; }
        if (x != cnt && found) { return i - 1; }
    }

    if (found)
        return i - 1;
    else
        return 0;
}

int countBillboards(int positions[], int dist, int n) {
    int billboard_count = 1, last = 0, i;

    for (i = 1; i < n; i++) {
        if (dist + positions[last] <= positions[i]) {
            last = i;
            billboard_count++;
        }
    }

    return billboard_count;
}

int compare(int *a, int *b) {
    return (*b < *a) - (*a < *b);
}
