//
// Created by Martin Skalický on 16/11/2018.
//

#include <sys/ioctl.h>
#include <zconf.h>
#include <printf.h>

char *ASCI = ".:-=+*#%@";
struct winsize w;

struct winsize getTermSize() {

    ioctl(STDOUT_FILENO, TIOCGWINSZ, &w);

    printf("lines %d\n", w.ws_row);
    printf("columns %d\n", w.ws_col);
    return w;
}

void fillArrayWithZeros(int rows, int cols, int array[rows][cols]) {
    for (int i = 0; i < rows; ++i) {
        for (int j = 0; j < cols; ++j) {
            array[i][j] = 0;
        }
    }
}

void printArray(int rows, int cols, int array[][cols]) {
    for (int i = 0; i < rows; ++i) {
        printf("%c", ASCI[array[i][0]]);
        for (int j = 1; j < cols; ++j) {
            printf(" %c", ASCI[array[i][j]]);
        }
        printf("\n");
    }
}

void updateArray(int coords[4], int rows, int cols, int array[][cols]) {
    for (int i = coords[1]-1; i < coords[3]; ++i) {
        for (int j = coords[0]-1; j < coords[2]; ++j) {
     //       printf("%d\n", array[i][j]);
            array[i][j] += 1;
        }
    }
}

void readInput(int rows, int cols, int array[][cols]) {
    int coords[4] = {0, 0, 0, 0};
    while (scanf(" [ %d ; %d ] [ %d ; %d ]", &coords[0], &coords[1], &coords[2], &coords[3]) == 4) {
        updateArray(coords, rows, cols, array);
        printArray(rows, cols, array);
    }
}


int main(void) {
    struct winsize win = getTermSize();
    int rows = 10;
    int cols =win.ws_col;
    //int cols = 40;
    int array[rows][cols];
    fillArrayWithZeros(rows, cols, array);

    printArray(rows, cols, array);
    readInput(rows, cols, array);

    return 0;
}