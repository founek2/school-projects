#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

int error() {
    printf("Nespravny vstup.\n");
    return 1;
}

char *reallocate(int *allocated, char *sequence) {
    *allocated *= 2;
    return (char *) realloc(sequence, *allocated * sizeof(char));
}

int toArab(char string) {
    switch (string) {
        case 73: //I
            return 1;
        case 86: //V
            return 5;
        case 88: //X
            return 10;
        case 76: //L
            return 50;
        case 67: //C
            return 100;
        case 68: //D
            return 500;
        case 77: //M
            return 1000;
        default:
            printf("bugggggy - unknown roman Number -> %c\n", string);
            return 111;
    }
}

int arabify3(char *array, int len) {
    int sum = 0;
    int i;
    if (len - 1 == 0)
        return toArab(array[0]);
    for (i = len - 1; i > 0; i -= 2) {
        int second = toArab(array[i]);
        int first = toArab(array[i - 1]);

        if (second > first)
            sum += second - first;
        else
            sum += first + second;
    }
    if (i == 0) {
        int first = toArab(array[i]);
        int second = toArab(array[i + 1]);
        if (second > first)
            sum -= first;
        else
            sum += first;
    }
    return sum;
}

int arabify2(char *array, int len) {
    int sum = 0;
    int i = 0;
    if (len - 1 == 0)
        return toArab(array[0]);
    while (i < len - 1) {
        int second = toArab(array[i + 1]);
        int first = toArab(array[i]);

        if (first >= second) {
            sum += first;
            i++;

        } else {
            i += 2;
            sum += second - first;
        }
    }
    if (i == len - 1) {
        int first = toArab(array[i]);

        sum += first;
    }
    return sum;
}

int isForbidStacking(char letter) {
    return letter == 'D' || letter == 'L' || letter == 'V';
}
//CIX

//IVIII
int isRomanValid(char *array, int len) {
    int sameCounter = 0;
    int passedHalf = 0;
    int sub = 0;
    for (int i = len - 1; i > 0; --i) {
        if (array[i] == array[i - 1])
            sameCounter++;
        else
            sameCounter = 0;

        if (sameCounter >= 3
            || (array[i] == array[i - 1] && isForbidStacking(array[i])))
            return 0;
        if (5 * toArab(array[i - 1]) == toArab(array[i])
            || 10 * toArab(array[i - 1]) == toArab(array[i])) {
            if (passedHalf)
                return 0;
            sub = 1;
        } else if (toArab(array[i - 1]) >= toArab(array[i])) {
            passedHalf = 1;
        } else
            return 0;

        if ((sub && isForbidStacking(array[i - 1])) || (passedHalf && sub))
            return 0;
    }

    return 1;
}

int isRomanValid2(char *array, int len) {
    int sameCounter = 0;
    int subBefore = 0;
    int tmp = 0;
    int i = 0;
    while (i < len - 1) {

        if (array[i] == array[i + 1])
            sameCounter++;
        else
            sameCounter = 0;

        if (sameCounter >= 3
            || (array[i] == array[i + 1] && isForbidStacking(array[i])))
            return 0;


        if (5 * toArab(array[i]) == toArab(array[i + 1])
            || 10 * toArab(array[i]) == toArab(array[i + 1])) {
            if (isForbidStacking(array[i]))
                return 0;
            int newNum = toArab(array[i + 1]) - toArab(array[i]);
            if (tmp == 0 || ((!subBefore && tmp >= newNum) || ((toArab(array[i - 1]) / 10 > newNum) && tmp > newNum))) {
                if (i > 0) {
                    if (array[i - 1] == array[i] || array[i - 1] == array[i + 1])
                        if (isForbidStacking(array[i - 1]))
                            return 0;
                }
                tmp = newNum;

                i += 2;
                subBefore = 1;
            } else
                return 0;

        } else if (toArab(array[i]) < toArab(array[i + 1])) {
            return 0;
        } else {
            //prvni je vetsi nez druhy
            if (tmp == 0 || !subBefore || (tmp > toArab(array[i]) && toArab(array[i - 1]) / 10 > toArab(array[i]))) {

                tmp = toArab(array[i]);
                subBefore = 0;
                ++i;
            } else
                return 0;
        }


    }
    if (len > 1 && i == len - 1) {
        if (subBefore) {
            if (toArab(array[i - 1]) / 10 > toArab(array[i])) {

            } else
                return 0;
        } else if (toArab(array[i - 1]) >= toArab(array[i])) {

        } else
            return 0;
    }

    return 1;
}

int loadRomanNumbers(int *sequenceLen, char **sequence) {
    *sequenceLen = 0;

    int allocated = 50;
    *sequence = (char *) calloc(allocated, sizeof(char));

    char letter = 0;
    while (scanf("%c", &letter) == 1) {
        if (letter == 'I' || letter == 'V' || letter == 'X' || letter == 'L' || letter == 'C' || letter == 'D' ||
            letter == 'M') {
            if (*sequenceLen == allocated)
                *sequence = (char *) reallocate(&allocated, *sequence);

            (*sequence)[(*sequenceLen)++] = letter;
        } else if (letter == '\n')
            break;
        else {
            return 0;
        }
    }
    if (*sequenceLen == 0)
        return 0;
    (*sequence)[*sequenceLen] = '\0';
    return 1;
}

char *globalPointer = NULL;
int globalLen = 0;

void findCombinationsRec(long long int count, char *array, int len, long long int accum, int *plusPositions,
                         int *passCounter, int quiet) {
    if (accum > count)
        return;
    if (len == 0) {
        if (accum == count) {
            ++*passCounter;
            if (!quiet) {
                printf("> %lld = ", count);
                for (int i = 0; i < globalLen; ++i) {
                    printf("%c", globalPointer[i]);
                    if (i < globalLen - 1 && plusPositions[i] == 1) {
                        printf("+");
                    }
                }

                printf("\n");
            }
        }
        return;
    }
    int actLen = 1;
    while (actLen <= len && isRomanValid2(array, actLen)) {
        plusPositions[globalLen - len + actLen - 1] = 1;
//        for (int i = 0; i < actLen; i++) {
//            printf("%c", array[i]);
//        }
        findCombinationsRec(count, &array[actLen], len - actLen, accum + arabify2(array, actLen), plusPositions,
                            passCounter, quiet);
        plusPositions[globalLen - len + actLen - 1] = 0;
        ++actLen;
        //       printf("\n");
    }
    if (actLen <= len) {

    }

}

int findCombinations(long long int count, char *array, int len, int quiet) {
    int *plusPositions = (int *) malloc(len * sizeof(int));
    int passCounter = 0;
    findCombinationsRec(count, array, len, 0, plusPositions, &passCounter, quiet);
    return passCounter;
}

int processOperations(int sequanceLen, char *sequence) {
    char newLine;
    long long int number = 0;

    while (1) {
        char buffer[10] = {0};
        int len = 0;
        char letter;
        for (int i = 0; i < 6; ++i) {
            int result = scanf("%c", &letter);
            if (result == EOF)
                return 1;
            if (letter == ' ') {
                break;
            }
            if (result == 1) {
                buffer[i] = letter;
                len++;
            } else
                break;
        }

        if (scanf(" %lld%c", &number, &newLine) == 2) {

        } else
            return 0;

        if (number <= 0)
            return 0;

        if (strncmp(buffer, "count", len) == 0) {
            int count = findCombinations(number, sequence, sequanceLen, 1);
            printf("%lld: %d\n", number, count);
        } else if (strncmp(buffer, "list", len) == 0) {
            int count = findCombinations(number, sequence, sequanceLen, 0);
            printf("%lld: %d\n", number, count);
        } else
            return 0;

    }
}

int main() {
    char *sequence;
    int sequenceLen;/*
#ifndef __PROGTEST__

#include "asserts.c"

#endif *//* __PROGTEST__ */
    printf("Sekvence:\n");
    if (!loadRomanNumbers(&sequenceLen, &sequence)) {
        return error();
    }
    globalPointer = sequence;
    globalLen = sequenceLen;
    printf("Operace:\n");
    if (!processOperations(sequenceLen, sequence)) {
        return error();
    }

    return 0;
}
