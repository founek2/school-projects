#include <stdio.h>

int loadInput(int *numSystem, long long int *min, long long int *max, char *operation) {
    char end;
    int result = scanf("r %d : < %lld ; %lld > %c %c ", numSystem, min, max, operation, &end);
    if (result == 4 && *min >= 0 && *max >= 0 && *min <= *max) {
        if (*operation == 'l' || *operation == 's' || *operation == 'z') {
            return 1;
        }
    }

    return 0;
}

long long int getZeroBitCount(long long int number) {
    long long int counter = 0;

    if (number == 0)
        return 1;

    while (number > 0) {
        long long int first_num = number & 1;

        if (!first_num)
            counter++;
        number >>= 1;
    }

    return counter;
}

long long int getHieghestZero(long long int number) {
    if (number == 0)
        return 1;
    long long int counter = 0;
    long long int temp = 0;
    while (number > 0) {
        long long int first_num = number & 1;

        number >>= 1;
        if (!first_num)
            counter++;
        else
            break;
    }

    while (number > 0) {
        long long int first_num = number & 1;

        if (!first_num)
            temp++;
        else if (temp > counter) {
            counter = temp;
            temp = 0;
        } else {
            temp = 0;
        }
        number >>= 1;
    }

    return counter;
}

long long int getBitCount(long long int number) {
    if (!number)
        return 1;

    long long int counter = 1;
    while (number >>= 1)
        counter++;

    return counter;
}


long long int passInterval(long long int min, long long int max, char operation, int base) {
    long long int counter = 0;


    while (min <= max) {
        int remainder, i = 0;

        long long int value = min;
        while (value > 0) {
            remainder = value % base; // Get next place value
            value /= base; // Move over value by one place
            //result[i++] = remainder; // Store the place value

            if (operation == 's') {

            } else if (operation == 'l') {
                i++;
            } else if (operation == 'z') {
                if (remainder == 0) {
                    i++;
                }
            }
        }

        counter += i;
        min++;
    }
    return counter;
}


int main() {
    long long int min = 1, max;
    int numSystem;
    char operation;

    printf("Zadejte interval:\n");
    if (!loadInput(&numSystem, &min, &max, &operation)) {
        printf("Nespravny vstup.\n");
        return 1;
    }

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

    return 0;
}


int getPositiveBitCount(unsigned int n) {

    // 10- 1010
    // 9 - 1001
    // 8 - 1000
    int counter = 0;
    while (n) {

        counter++;
        n &= (n - 1);
    }
    return counter;
}
