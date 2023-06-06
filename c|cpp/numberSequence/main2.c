#include <stdio.h>

char leadingChar = -1;
int leadingResult = -3;
int loadInput(int *numSystem, long long int *min, long long int *max, char *operation) {
    char startC = 0;
    int result;
    if (leadingChar > -1){
        startC = leadingChar;
        result = leadingResult;
    } else {
        result = scanf(" %c ", &startC);
    }

    if (result == EOF)
        return 0;

    if (startC == 'r') {
        char first, second;
        result = scanf(" %d %c %c", numSystem, &first, &second);

        if (result != 3 || first != ':' || second != '<'){
            printf("Nespravny vstup.\n");
            return 0;
        }

    } else if (startC == '<'){
        *numSystem = 10;
    } else {
        printf("Nespravny vstup.\n");
        return 0;
    }

    result = scanf(" %lld ; %lld > %c ", min, max, operation);

    leadingResult = scanf(" %c ", &leadingChar);

    if (leadingChar == 'r' || leadingChar == '<' || leadingResult == -1){
        if (result == 3 && *min >= 0 && *max >= 0 && *min <= *max
            && (*operation == 'l' || *operation == 's' || *operation == 'z')
            && *numSystem >= 2 && *numSystem <= 36) {
            return 1;
        }
    }

    printf("Nespravny vstup.\n");
    return 0;
}


long long int passInterval(long long int min, long long int max, char operation, int base) {
    long long int counter = 0;

    while (min <= max) {
        long long int remainder, i = 0;
        int heighestNull =0;

        long long int value = min;
        if (value == 0) {
            i++;
        }
        while (value > 0) {
            remainder = value % base; // Get next place value
            value /= base; // Move over value by one place

            if (operation == 's') {
                if (remainder == 0) {
                    heighestNull++;
                } else {
                    if (i < heighestNull) {
                        i = heighestNull;
                    }
                    heighestNull = 0;
                }
            } else if (operation == 'l') {
                i++;
            } else if (operation == 'z') {
                if (remainder == 0) {
                    i++;
                }
            }
        }
        if (operation != 's'){
            counter += i;
        } else if (counter < i){
            counter = i;
        }
        min++;
    }
    return counter;
}


int main() {
    long long int min, max;
    int numSystem;
    char operation;

    printf("Zadejte intervaly:\n");
    while (loadInput(&numSystem, &min, &max, &operation)) {

        switch (operation) {
            case 'l': {
                long long int result = passInterval(min, max, operation, numSystem);
                printf("Cifer: %lld\n", result);
                break;
            }
            case 's': {
                long long int result = passInterval(min, max, operation, numSystem);

                printf("Sekvence: %lld\n", result);
                break;
            }
            case 'z': {
                long long int result = passInterval(min, max, operation, numSystem);
                printf("Nul: %lld\n", result);
                break;
            }
        }
    }

    return 0;
}

