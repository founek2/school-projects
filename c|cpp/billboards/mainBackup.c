#include <stdio.h>
#include <stdlib.h>

#define LEN 1000000

int error() {
    printf("Nespravny vstup.\n");
    return 1;
}

typedef struct {
    int position;
    int occupied;
} BillboardPos;

int loadPositions(BillboardPos array[LEN], int *posCount) {
    char openBracket = 0;
    if (scanf(" %c ", &openBracket) != 1 || openBracket != '{')
        return 0;

    int position = 0;
    char closeBracket = 0;

    int counter = 0;
    while (scanf(" %d ", &position) == 1 && scanf(" %c", &closeBracket) == 1) {
        if (!(closeBracket == '}' || closeBracket == ',') || position < 0 || counter > LEN -1 ) {
            return 0;
        }

        array[counter].position = position;
        counter++;

        if (closeBracket == '}') {
            break;
        }
    }
    *posCount = counter;
    char newLine = getchar();
    if (newLine != '\n')
        return 0;

    return 1;
}

int tryPlace(int posCount, BillboardPos positions[LEN], int minLen) {
    int placedCount = 0;

    int prevDist = 0;
    for (int i = 1; i < posCount - 1; ++i) {

        int distance = prevDist + positions[i].position - positions[i - 1].position;
        int distanceToLast = positions[posCount - 1].position - positions[i].position;
        if (distanceToLast < minLen) {
            break;
        } else {
            if (distance >= minLen) {
                placedCount++;
                prevDist = 0;
            } else {
                prevDist = distance;
            }
        }

    }

    return placedCount;
}

int distributeBillboards(int billboardCount, int posCount, BillboardPos positions[LEN]) {


    int minLen = positions[posCount - 1].position;
    int placed = tryPlace(posCount, positions, minLen);

    while (placed < billboardCount - 2) {
        minLen -= 1;
        if (minLen < 0)
            return -2;
        placed = tryPlace(posCount, positions, minLen);
    }

    return minLen;
}

int checkWeird(int billboardCount, int posCount, BillboardPos positions[LEN]) {
    if (billboardCount > posCount) {
        return -2;
    } else if (billboardCount == 0 || billboardCount == 1) {
        return -1;
    } else if (billboardCount == 2) {
        return positions[posCount - 1].position - positions[0].position;
    }
    return -3;
}

int cmpfunc(const void *a, const void *b) {
    BillboardPos *positionA = (BillboardPos *) a;
    BillboardPos *positionB = (BillboardPos *) b;

    return (positionA->position - positionB->position);
}

int main() {
    int checkInput = 0;
    int posCount = 0;
    BillboardPos positions[LEN];

/*  positions[0].position = 0;
    positions[1].position = 1;
    positions[2].position = 2;
    positions[3].position = 1;
    positions[4].position = 2;
    posCount = 5;*/

    printf("Mozna umisteni:\n");
    checkInput = loadPositions(positions, &posCount);
    if (!checkInput)
        return error();

    printf("Pocet billboardu:\n");

    int billboardCount = -1;
    char newLine = 0;

    while (scanf(" %d%c", &billboardCount, &newLine) == 2) {
        if (billboardCount < 0)
            return error();
        if (newLine != '\n')
            return error();

        qsort(positions, posCount, sizeof(BillboardPos), cmpfunc);
        int minDistance = checkWeird(billboardCount, posCount, positions);
        if (minDistance == -3) {
            minDistance = distributeBillboards(billboardCount, posCount, positions);
        }

        if (minDistance == -1)
            printf("Vzdalenost: inf\n");
        else if (minDistance == -2)
            printf("N/A\n");
        else
            printf("Vzdalenost: %d\n", minDistance);
    }
    if (billboardCount == -1)
        return error();

    return 0;
}
