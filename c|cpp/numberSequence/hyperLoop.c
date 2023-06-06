#ifndef __PROGTEST__

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <assert.h>

#endif /* __PROGTEST__ */

void sortParams(unsigned int *a, unsigned int *b, unsigned int *c) {
    unsigned int min = *a;
    min = min > *b
          ? *b > *c
            ? *c : *b
          : min > *c ? *c : min;
    unsigned int max = *a;
    max = max < *b
          ? *b < *c
            ? *c : *b
          : max < *c ? *c : max;

    unsigned int mid = *a + *b + *c - min - max;
    if ( min == max) {
        *a = max;
        *b = 0;
        *c = 0;
    } else if (max == mid) {
        *a = max;
        *b = min;
        *c = 0;
    } else if (mid == min) {
        *a = max;
        *b = mid;
        *c = 0;
    }else {
        *a = max;
        *b = mid;
        *c = min;
    }
}

unsigned long long int findPosibilities(
        unsigned long long int length,
        unsigned int tube1,
        unsigned int tube2,
        unsigned int tube3,
        unsigned int *count1,
        unsigned int *count2,
        unsigned int *count3
) {

 /*   if (tube1 == 0 && tube2 == 0 && tube3 == 0 && length == 0) {
        return 1;
    }*/

    if (tube1 == 0 && tube2 == 0 && tube3 == 0) {
        return 0;
    }

    if (tube2 == 0 && tube3 == 0) {

        if (length % tube1 == 0){
            *count1 =  (unsigned int) length / tube1;
            return 1;
        }
        return 0;
    }

    if (tube3 == 0) {
        unsigned long long int counter = 0;
        int firstPass = 1;

        for (unsigned int i = 0; i <= length / tube1; ++i) {
            if ((length - tube1 * i) % tube2 == 0) {
                counter++;
                if (firstPass) {
                    *count1 = i;
                    *count2 = (unsigned int) (length - tube1 * i) / tube2;
                    firstPass = !firstPass;
                }
            }

        }

        return counter;
    }

    unsigned long long int counter = 0;
    int firstPass = 1;

    unsigned long long int rest = length / tube1;
    for (unsigned int i = 0; i <= rest; i++) {
        unsigned long long int rest2 = (length - i * tube1) / tube2;
        for (unsigned int j = 0; j <= rest2; ++j) {

            if (length >= i * tube2 && length >= i * tube1 + j * tube2) {
                unsigned long long int newLen = length - i * tube1 - j * tube2;
                if (newLen % tube3 == 0) {

                    counter++;
                    if (firstPass) {
                        *count1 = i;
                        *count2 = j;
                        *count3 = (unsigned int) (length - i * tube1 - j * tube2) / tube3;
                        firstPass = !firstPass;
                    }
                }
            }

        }
    }

    return counter;
}

unsigned long long int hyperloop(unsigned long long int length,
                                 unsigned int s1,
                                 unsigned int s2,
                                 unsigned int s3,
                                 unsigned int bulkhead,
                                 unsigned int *c1,
                                 unsigned int *c2,
                                 unsigned int *c3) {
    if (bulkhead > length)
        return 0;

    if (length > 0 && bulkhead == length){
        *c1 = 0;
        *c2 = 0;
        *c3 = 0;
        return 1;
    }

    unsigned long long int restLength = length - bulkhead;
    unsigned int tube1 = (s1 == 0) ? 0 : (s1 + bulkhead);
    unsigned int tube2 = (s2 == 0) ? 0 : (s2 + bulkhead);
    unsigned int tube3 = (s3 == 0) ? 0 : (s3 + bulkhead);

    unsigned int s1b = (s1 == 0) ? 0 : (s1 + bulkhead);
    unsigned int s2b = (s2 == 0) ? 0 : (s2 + bulkhead);
    unsigned int s3b = (s3 == 0) ? 0 : (s3 + bulkhead);

    sortParams(&tube1, &tube2, &tube3);

    unsigned int count1 = 0;
    unsigned int count2 = 0;
    unsigned int count3 = 0;
    unsigned long long int combinations = findPosibilities(restLength, tube1, tube2, tube3, &count1, &count2, &count3);

    // přerovnat count do c
    if (combinations > 0) {
        if (tube1 == s1b) {
            *c1 = count1;
            if (tube2 == s2b) {
                *c2 = count2;
                *c3 = count3;
            } else if (tube2 == s3b) {
                *c3 = count2;
                *c2 = count3;
            } else {
                *c3 = 0;
                *c2 = 0;
            }
        } else if (tube1 == s2b) {
            *c2 = count1;
            if (tube2 == s1b) {
                *c1 = count2;
                *c3 = count3;
            } else if (tube2 == s3b) {
                *c3 = count2;
                *c1 = count3;
            } else {
                *c3 = 0;
                *c2 = 0;
            }
        } else if (tube1 == s3b) {
            *c3 = count1;
            if (tube2 == s1b) {
                *c1 = count2;
                *c2 = count3;
            } else if (tube2 == s2b) {
                *c2 = count2;
                *c1 = count3;
            } else {
                *c3 = 0;
                *c2 = 0;
            }
        }
    }

    return combinations;
}

#ifndef __PROGTEST__

int main(int argc, char *argv[]) {
    unsigned int c1, c2, c3;

    assert (hyperloop(100, 4, 7, 5, 0, &c1, &c2, &c3) == 42
            && 4 * c1 + 7 * c2 + 5 * c3 + 0 * (c1 + c2 + c3 + 1) == 100);
    assert (hyperloop(12345, 8, 3, 11, 3, &c1, &c2, &c3) == 82708
            && 8 * c1 + 3 * c2 + 11 * c3 + 3 * (c1 + c2 + c3 + 1) == 12345);
    c1 = 8;
    c2 = 9;
    c3 = 10;
    assert (hyperloop(127, 12, 8, 10, 0, &c1, &c2, &c3) == 0
            && c1 == 8
            && c2 == 9
            && c3 == 10);
    assert (hyperloop(127, 12, 8, 10, 3, &c1, &c2, &c3) == 4
            && 12 * c1 + 8 * c2 + 10 * c3 + 3 * (c1 + c2 + c3 + 1) == 127);
    assert (hyperloop(100, 35, 0, 0, 10, &c1, &c2, &c3) == 1
            && c2 == 0
            && c3 == 0
            && 35 * c1 + 10 * (c1 + 1) == 100);
    assert (hyperloop(100, 35, 0, 35, 10, &c1, &c2, &c3) == 1
            && c2 == 0
            && 35 * c1 + 35 * c3 + 10 * (c1 + c3 + 1) == 100);
    assert (hyperloop(100, 35, 35, 35, 10, &c1, &c2, &c3) == 1
            && 35 * c1 + 35 * c2 + 35 * c3 + 10 * (c1 + c2 + c3 + 1) == 100);
    c1 = 42;
    c2 = 43;
    c3 = 44;
    assert (hyperloop(9, 1, 2, 3, 10, &c1, &c2, &c3) == 0
            && c1 == 42
            && c2 == 43
            && c3 == 44);
    return 0;
}

#endif /* __PROGTEST__ */
