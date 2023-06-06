#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <float.h>

#define LEN 26
const int A = 0, B = 1, C = 2, D = 3, E = 4, F = 5, G = 6, H = 7, I = 8, J = 9, K = 10, L = 11, M = 12, N = 13,
        O = 14, P = 15, Q = 16, R = 17, S = 18, T = 19, U = 20, V = 21, W = 22, X = 23, Y = 24, Z = 25;

typedef struct {
    int from;
    int to;
    double price;
} Toll;

Toll createToll(int from, int to, double price){
    Toll toll;
    toll.from = from;
    toll.to = to;
    toll.price = price;

    return toll;
}

Toll *tollArray[LEN];
int allocated[LEN][3] = {{0, 0}}; // fill rest array with static default zeros

Toll *tempArray[LEN];
int tempAllocated[LEN][3] = {{0, 0}};

void clearTollArray(){
    for (int letter = 0; letter < 26; ++letter) {
        free(tollArray[letter]);
    }
}

void clearDynamics() {
    for (int letter = 0; letter < 26; ++letter) {
        free(tollArray[letter]);
        free(tempArray[letter]);
    }
}

int error() {
    printf("Nespravny vstup.\n");
    clearDynamics();
    return 1;
}

Toll *reallocate(Toll **array, int len) {
    return (Toll *) realloc(*array, len * sizeof(Toll));
}

void addToDyn(Toll **array, int lens[2], int from, int to, double price) {
    if (lens[1] == 0) {
        lens[1] = 10;
        *array = (Toll *) malloc(lens[1] * sizeof(Toll));
    } else if (lens[0] == lens[1]) {
        lens[1] *= 2;
        *array = reallocate(&*array, lens[1]);
    }
    Toll *pointer = *array;
    pointer[lens[0]++] = createToll(from, to, price);
}

int loadTollUpdates() {
    char openBracket;
    if (scanf(" %c", &openBracket) != 1 || openBracket != '{')
        return 0;

    int from, to;
    double price;
    char letter = 0, closeBracket;
    while (scanf(" [ %d - %d : %c = %lf ] %c", &from, &to, &letter, &price, &closeBracket) == 5) {

        if (letter < 65 || letter > 90 || !(closeBracket == '}' || closeBracket == ',')
            || price < 0 || from < 0 || to < 0 || from >= to)
            return 0;

        int index = letter - 65;
        if (allocated[index][1] == 0) {
            tollArray[index] = (Toll *) malloc(10 * sizeof(Toll));
            allocated[index][1] = 10;
        } else if (allocated[index][0] == allocated[index][1]) {
            allocated[index][1] *= 2;
            tollArray[index] = reallocate(&tollArray[index], allocated[index][1]);
        }

        tollArray[index][allocated[index][0]++] = createToll(from, to, price);

        if (closeBracket == '}')
            break;
    }

    if (letter == 0)
        return 0;

    return 1;
}

void changesLogic(Toll *change, Toll *old, int letter) {

    if (old->from >= change->from &&
        old->to <= change->to) { // jsou identické nebo j je vnořené
        old->to = -1;
        old->from = -1;
    } else if (old->from < change->from &&
               old->to > change->to) { // i je vnoreny -> dělý to na dva menší, nebo je stejný???

        //addToDyn(&tempArray[letter], tempAllocated[letter], change->to, old->to, old->price);
        Toll newToll = {change->to, old->to, old->price};

        for (int i = 0; i < tempAllocated[letter][0]; ++i) {
            changesLogic(&tempArray[letter][i], &newToll, letter);
        }
        addToDyn(&tempArray[letter], tempAllocated[letter], newToll.from, newToll.to, newToll.price);
        old->to = change->from;
    } else if (old->from < change->from && old->to <= change->to &&
               old->to > change->from) { // j je ze spoda menší, vršek stejný nebo v intervalu -> zbyde spodek
        old->to = change->from;
    } else if (old->from >= change->from && old->from < change->to && old->to >
                                                                      change->to) { // j je ze spoda stejné nebo v intervalu, vršek má vetší -> zbyde vrch
        old->from = change->to;
    } else {
        // printf("some magic happend\n");
    }

}

int cmpfunc(const void *a, const void *b) {
    Toll *tollA = (Toll *) a;
    Toll *tollB = (Toll *) b;

    return ((tollA->to + tollA->from) - (tollB->from + tollB->to));
}

void processUpdates(Toll *tollArray[LEN]) {
    for (int letter = 0; letter < 26; ++letter) {
        if (allocated[letter] != NULL) {

            int numOfChanges = allocated[letter][0];
            for (int i = numOfChanges - 1; i >= 0; --i) {
                Toll *toll = tollArray[letter];

/*                int addedToDyn = 0;
                for (int k = dynCounter; k < tempAllocated[letter][0]; ++k) {
                    changesLogic(&toll[i], &tempArray[letter][k], letter, &addedToDyn);
                }
                if (addedToDyn)
                    dynCounter++;*/

                for (int j = i -1; j >= 0; --j) {

                    // projde jen vnořený/stejný interval
                    changesLogic(&toll[i], &toll[j], letter);

                }
            }
        }
    }

    //TODO odstranit -1 - vynulované hodnoty, snížit countery podle toho a seřadit

    for (int letter = 0; letter < 26; ++letter) {
        if (tempAllocated[letter][0] > 0) {
            int reqLen = allocated[letter][0] + tempAllocated[letter][0];
            if (reqLen > allocated[letter][1]) {
                tollArray[letter] = reallocate(&tollArray[letter], reqLen);
                allocated[letter][1] = reqLen;
            }

            memcpy(tollArray[letter] + allocated[letter][0], tempArray[letter],
                   tempAllocated[letter][0] * sizeof(Toll));
            allocated[letter][0] += tempAllocated[letter][0];

            free(tempArray[letter]);
        }


        qsort(tollArray[letter], allocated[letter][0], sizeof(Toll), cmpfunc);
    }

    //TODO remove
/*    for (int letter = 0; letter < 26; ++letter) {

        if (tollArray[letter] != NULL) {
            printf("Alokované %c:\n", letter + 65);
            for (int l = 0; l < allocated[letter][0]; ++l) {
                Toll *toll = tollArray[letter];
                printf(" [ %d - %d, cena %.6lf ]", toll[l].from, toll[l].to, toll[l].price);
            }
            printf("\n");
        }
    }
    printf("\n");*/
}

int calculatePrices(int from, int to) {
    int passed = 0;

    for (int letter = 0; letter < 26; ++letter) {
        double price = 0;

        if (from < to) {
            for (int i = allocated[letter][0] - 1; i >= 0; --i) {
                Toll toll = tollArray[letter][i];

                if (from >= toll.from && to <= toll.to) {
                    price = (to - from) * toll.price;
                    break;
                } else if (from >= toll.from && from < toll.to) {
                    price += (toll.to - from) * toll.price;
                } else if (to <= toll.to && to > toll.from) {
                    price += (to - toll.from) * toll.price;
                } else if (from < toll.from && to > toll.to) {
                    price += (toll.to - toll.from) * toll.price;
                }


            }
        } else {
            // from > to
            // from = 33
            // to = 2
            for (int i = 0; i < allocated[letter][0]; ++i) {
                Toll toll = tollArray[letter][i];

                if (to >= toll.from && from <= toll.to) {
                    price = (from - to) * toll.price;
                    break;
                } else if (to >= toll.from && to < toll.to) { // počátek je v intervalu
                    price += (toll.to - from) * toll.price;
                } else if (from <= toll.to && from > toll.from) { // vrch je v intervalu
                    price += (from - toll.from) * toll.price;
                } else if (to < toll.from && from > toll.to) { // toll je v rozmezí to - from
                    price += (toll.to - toll.from) * toll.price;
                }
            }
        }
        if (price > DBL_EPSILON) {
            if (passed) {
                printf(", %c=%.6lf", letter + 65, price);
            } else {
                passed = 1;
                printf(" %c=%.6lf", letter + 65, price);
            }

        }
    }

    printf("\n");
    return 1;
}

int main() {
    printf("Zmeny:\n");
    if (!loadTollUpdates())
        return error();

    processUpdates(tollArray);

    int from, to;
    char newLine;
    int result;

    printf("Hledani:\n");
    while ((result = scanf(" %d %d%c", &from, &to, &newLine)) && result == 3) {
        if (newLine != '\n' || from < 0 || to < 0 || from == to)
            return error();

        printf("%d - %d:", from, to);
        calculatePrices(from, to);
    }
    if (result > 0 && result < 3)
        return error();
    //TODO dodělat free paměti pro dyn pole v tollArray a dynArray
    clearTollArray();
    return 0;
}
