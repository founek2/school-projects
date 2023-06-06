//
// Created by Martin Skalický on 30/11/2018.
//
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <float.h>

#define LEN 26

void printTollArray();
int calculatePrices(int from, int to);

typedef struct {
    int from;
    int to;
    double price;
} Toll;

void addNewToll(Toll newToll, int letter);

int cmpfunc(const void *a, const void *b);


Toll createToll(int from, int to, double price) {
    Toll toll;
    toll.from = from;
    toll.to = to;
    toll.price = price;

    return toll;
}

Toll *tollArray[LEN];
int allocated[LEN][3] = {{0, 0}}; // fill rest array with static default zeros

void clearTollArray() {
    for (int letter = 0; letter < 26; ++letter) {
        free(tollArray[letter]);
    }
}


int error() {
    printf("Nespravny vstup.\n");
    clearTollArray();
    return 1;
}

Toll *reallocate(Toll **array, int len) {
    return (Toll *) realloc(*array, len * sizeof(Toll));
}

Toll *remove_element(Toll *array, int arraySize[], int indexToRemove) {
    Toll *temp = (Toll*) malloc(arraySize[1] * sizeof(Toll)); // allocate an array with a size 1 less than the current one

    if (indexToRemove != 0)
        memcpy(temp, array, indexToRemove * sizeof(Toll)); // copy everything BEFORE the index

    if (indexToRemove != (arraySize[1] - 1))
        memcpy(temp + indexToRemove, array + indexToRemove + 1,
               (arraySize[0] - indexToRemove - 1) * sizeof(Toll)); // copy everything AFTER the index

    free(array);
    --arraySize[0];
    return temp;
}

void processUpdate(int letter, Toll *change) {

    for (int i = allocated[letter][0] - 1; i >= 0 ;--i) {
        Toll *old = &tollArray[letter][i];

        if (old->from >= change->from &&
            old->to <= change->to) { // jsou identické nebo j je vnořené

            //TODO remove?
            tollArray[letter] = remove_element(tollArray[letter], allocated[letter], i);

        } else if (old->from < change->from &&
                   old->to > change->to) { // i je vnoreny -> dělý to na dva menší, nebo je stejný???

            //addToDyn(&tempArray[letter], tempAllocated[letter], change->to, old->to, old->price);
            Toll newToll = {change->to, old->to, old->price};
            old->to = change->from;
            addNewToll(newToll, letter);

        } else if (old->from < change->from && old->to <= change->to &&
                   old->to > change->from) { // j je ze spoda menší, vršek stejný nebo v intervalu -> zbyde spodek
            old->to = change->from;
        } else if (old->from >= change->from && old->from < change->to && old->to >
                                                                          change->to) { // j je ze spoda stejné nebo v intervalu, vršek má vetší -> zbyde vrch
            old->from = change->to;
        }
    }


    addNewToll(*change, letter);
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

        Toll newChange = createToll(from, to, price);
        processUpdate(letter - 65, &newChange);

        if (closeBracket == '}')
            break;
    }

    if (letter == 0 || closeBracket != '}')
        return 0;

    return 1;
}

int main() {
    printf("Zmeny:\n");
    if (!loadTollUpdates())
        return error();

    for (int letter = 0; letter < 26; ++letter) {
        qsort(tollArray[letter], allocated[letter][0], sizeof(Toll), cmpfunc);
    }

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

    if (result >= 0 && result < 3)
        return error();
    //TODO dodělat free paměti pro dyn pole v tollArray a dynArray
    clearTollArray();
    return 0;
}


void printTollArray() {
    for (int letter = 0; letter < 26; ++letter) {

        if (tollArray[letter] != NULL) {
            printf("Alokované %c:\n", letter + 65);
            for (int l = 0; l < allocated[letter][0]; ++l) {
                Toll *toll = tollArray[letter];
                printf(" [ %d - %d, cena %.6lf ]", toll[l].from, toll[l].to, toll[l].price);
            }
            printf("\n");
        }
    }
    printf("\n");
}

int cmpfunc(const void *a, const void *b) {
    Toll *tollA = (Toll *) a;
    Toll *tollB = (Toll *) b;

    return ((tollA->to + tollA->from) - (tollB->from + tollB->to));
}

void addNewToll(Toll newToll, int letter) {
    if (allocated[letter][1] == 0) {
        tollArray[letter] = (Toll *) malloc(5000 * sizeof(Toll));
        allocated[letter][1] = 5000;
    } else if (allocated[letter][0] == allocated[letter][1]) {
        allocated[letter][1] *= 2;
        tollArray[letter] = reallocate(&tollArray[letter], allocated[letter][1]);
    }

    tollArray[letter][allocated[letter][0]++] = newToll;
}

int checkInterval(Toll change, Toll old) {
    if ((old.from >= change.from && old.to <= change.to)// jsou identické nebo j je vnořené
        || (old.from < change.from && old.to > change.to)// i je vnoreny . dělý to na dva menší, nebo je stejný???
        || (old.from < change.from && old.to <= change.to && old.to > change.from) // j je ze spoda menší, vršek stejný nebo v intervalu . zbyde spodek
        || (old.from >= change.from && old.from < change.to && old.to > change.to)) { // j je ze spoda stejné nebo v intervalu, vršek má vetší . zbyde vrch
        return 1;
    } else if (change.from < old.to) {
        return 2;
    }
    return 0;
}

int binarySearch(Toll *array, int leftIndex, int rightIndex, Toll findToll) {
    if (rightIndex == leftIndex && checkInterval(array[leftIndex], findToll) != 1)
        return -1;
    int middleIndex = leftIndex + (rightIndex - leftIndex) / 2;
    if (checkInterval(array[middleIndex], findToll) == 1)
        return middleIndex;
    else if (checkInterval(array[middleIndex], findToll) == 2)
        return binarySearch(array, middleIndex + 1, rightIndex, findToll);
    else return binarySearch(array, leftIndex, leftIndex > middleIndex - 1 ? leftIndex : middleIndex - 1, findToll);

}
void calcFromTo(int from, int to, double * price, Toll toll){
    if (from >= toll.from && to <= toll.to) {
        *price = (to - from) * toll.price;
    } else if (from >= toll.from && from < toll.to) {
        *price += (toll.to - from) * toll.price;
    } else if (to <= toll.to && to > toll.from) {
        *price += (to - toll.from) * toll.price;
    } else if (from < toll.from && to > toll.to) {
        *price += (toll.to - toll.from) * toll.price;
    }
}

void calcToFrom(int from, int to, double * price, Toll toll){
    if (to >= toll.from && from <= toll.to) {
        *price = (from - to) * toll.price;
    } else if (to >= toll.from && to < toll.to) { // počátek je v intervalu
        *price += (toll.to - to) * toll.price;
    } else if (from <= toll.to && from > toll.from) { // vrch je v intervalu
        *price += (from - toll.from) * toll.price;
    } else if (to < toll.from && from > toll.to) { // toll je v rozmezí to - from
        *price += (toll.to - toll.from) * toll.price;
    }
}

int calculatePrices(int from, int to) {
    int passed = 0;

    for (int letter = 0; letter < 26; ++letter) {
        double price = 0;

        if (tollArray[letter] != NULL){
            Toll * array = tollArray[letter];
            Toll origToll = createToll(from, to, 0);
            int index = binarySearch(tollArray[letter], 0, allocated[letter][0] -1, origToll);
            if (index != -1){
                for (int i = index; i >= 0 ; --i) {
                    if (checkInterval(array[i], origToll) == 1) {
                        Toll toll = array[i];
                        if (from < to) {
                            calcFromTo(from, to, &price, toll);
                        } else {
                            calcToFrom(from, to, &price, toll);
                        }
                    } else break;
                }

                for (int i = index + 1; i < allocated[letter][0]; ++i) {
                    if (checkInterval(array[i],origToll) == 1) {
                        Toll toll = array[i];
                        if (from < to) {
                            calcFromTo(from, to, &price, toll);
                        } else {
                            calcToFrom(from,to, &price, toll);
                        }
                    } else break;
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
