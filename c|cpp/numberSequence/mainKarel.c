#include <stdio.h>

int power(int x, int n) {
    int temp = x;
    for (int i = 1; i < n; i++){
        temp *= x;
    }
    return temp;
}

int main() {
    printf("%d",power(2, 4));
}