#include <stdio.h>

int loadInput(long long int *min, long long int *max, char *operation) {
    char end;
    int result = scanf(" < %lld ; %lld > %c %c ", min, max, operation, &end);
    if (result == 3 && *min >= 0 && *max >= 0 && *min <= *max) {
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

long long int passInterval(long long int min, long long int max, long long int (*function)(long long int)) {
    long long int counter = 0;

    while (min <= max) {
        counter += function(min);
        min ++;
    }
    return counter;
}

int main() {
    long long int min = 1, max;
    char operation;

    printf("Zadejte interval:\n");
    if (!loadInput(&min, &max, &operation)) {
        printf("Nespravny vstup.\n");
        return 1;
    }

    switch (operation) {
        case 'l': {
            long long int result = passInterval(min, max, getBitCount);
            printf("Cifer: %lld\n", result);
            break;
        }
        case 's': {
            long long int result = 0;

            while (min <= max) {
                long long int num = getHieghestZero(min);
                if (num > result)
                    result = num;
                min ++;
            }

            printf("Sekvence: %lld\n", result);
            break;
        }
        case 'z': {
            long long int result = passInterval(min, max, getZeroBitCount);
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
