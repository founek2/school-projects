//
// Created by Martin Skalický on 05/12/2018.
//

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define DEFAULT_ALOC 100

typedef struct {
    char letter;
    int occupied;
} Point;

typedef struct {
    int len;
    int used;
} Meta;

Meta *metaWords;

void printAllUnusedPoints(int colsCount, int rowsCount, Point **matrix);

char **loadWords(int *wordCounter);

void checkAllDirections(char *word, Point **matrix, int cell, int row, int rowsCount, int colsCount, int wordNum);

int findAllWords(int colsCount, int rowsCount, Point **matrix, int wordsCount, char **words);

Point **loadMatrix(int *rowsCount, int *colsCount);

int error() {
    printf("Nespravny vstup.\n");
    return 1;
}


void freeMatrix(Point **matrix, int rowCount) {
    for (int i = 0; i < rowCount; ++i) {
        free(matrix[i]);
    }
    free(matrix);
}

void freeWords(char **words, int rows) {
    for (int i = 0; i < rows; ++i) {
        free(words[i]);
    }
    free(words);
    free(metaWords);
}


int main() {
    printf("Zadejte osmismerku:\n");

    int colsCount, rowsCount;
    Point **matrix = loadMatrix(&rowsCount, &colsCount);
    if (matrix == NULL || rowsCount == 0) {
        return error();
    }

    int wordCount;
    char **words = loadWords(&wordCount);

    if (!findAllWords(colsCount, rowsCount, matrix, wordCount, words)) {
        freeMatrix(matrix, rowsCount);
        freeWords(words, wordCount);
        return 0;
    }
    printf("Vysledek:\n");
    printAllUnusedPoints(colsCount, rowsCount, matrix);

    freeMatrix(matrix, rowsCount);
    freeWords(words, wordCount);
}

Point *reallocatePoints(Point *row, int len) {
    return (Point *) realloc(row, len * sizeof(Point));
}

void addToMatrix(char value, int index, int row, Point **matrix) {
    Point point;
    point.letter = value;
    point.occupied = 0;
    matrix[row][index] = point;
}

Point **loadMatrix(int *rowsCount, int *colsCount) {
    *rowsCount = 0;
    *colsCount = 0;

    int allocatedRows = DEFAULT_ALOC;
    Point **matrix = (Point **) malloc(allocatedRows * sizeof(Point *));

    char letter = 0;
    int allocatedCols = 0;
    int localColCounter = 0;

    int firstWord = 1;
    int emptyLine = 0;
    while (scanf("%c", &letter) == 1) {
        if ((letter >= 'a' && letter <= 'z') || letter == '.') {
            if (*rowsCount == allocatedRows - 1) {
                allocatedRows *= 2;
                matrix = (Point **) realloc(matrix, allocatedRows * sizeof(Point *));
            }
            if (allocatedCols == 0) {
                allocatedCols = DEFAULT_ALOC;
                matrix[*rowsCount] = (Point *) malloc(allocatedCols * sizeof(Point));
            }
            if (allocatedCols == localColCounter) {
                allocatedCols *= 2;
                matrix[*rowsCount] = reallocatePoints(matrix[*rowsCount], allocatedCols);
            }
            addToMatrix(letter, localColCounter++, *rowsCount, matrix);
            if (firstWord)
                ++*colsCount;
        } else if (letter == '\n') {
            if (firstWord) firstWord = 0;
            if (allocatedCols == 0) {
                emptyLine = 1;
                break;
            }
            ++*rowsCount;

            if (localColCounter != *colsCount) {
                freeMatrix(matrix, *rowsCount);
                return NULL;
            }
            allocatedCols = 0;
            localColCounter = 0;
        } else {
            freeMatrix(matrix, *rowsCount);
            return NULL;
        }
    }

    if (emptyLine)
        return matrix;

    freeMatrix(matrix, *rowsCount);
    return NULL;
}

char **loadWords(int *wordCounter) {
    *wordCounter = 0;
    int allocatedRows = DEFAULT_ALOC;
    char **words = 0;

    words = (char **) malloc(allocatedRows * sizeof(*words));
    metaWords = (Meta *) malloc(allocatedRows * sizeof(Meta));

    char letter = 0;
    int allocatedCols = DEFAULT_ALOC;
    int colCounter = 0;
    while (scanf("%c", &letter) == 1) {
        if (letter != '\n') {
            if (*wordCounter == allocatedRows - 1) {
                allocatedRows *= 2;
                char **wordsTmp = (char **) realloc(words, allocatedRows * sizeof(*words));
                words = wordsTmp;
                metaWords = (Meta *) realloc(metaWords, allocatedRows * sizeof(Meta));
            }
            if (colCounter == 0)
                words[*wordCounter] = (char *) malloc(allocatedCols * sizeof(char));
            if (colCounter == allocatedCols - 1) { // -1 aby bylo místo na \0
                allocatedCols *= 2;
                words[*wordCounter] = (char *) realloc(words[*wordCounter], allocatedCols * sizeof(char));
            }
            words[*wordCounter][colCounter++] = letter;
        } else {
            if (colCounter != 0) {
                metaWords[*wordCounter].used = 0;
                metaWords[*wordCounter].len = colCounter;
                words[*wordCounter][colCounter] = '\0';
                ++*wordCounter;

                allocatedCols = DEFAULT_ALOC;
                colCounter = 0;
            }
        }
    }

    return words;
}

int findAllWords(int colsCount, int rowsCount, Point **matrix, int wordsCount, char **words) {
    for (int wordNum = 0; wordNum < wordsCount; ++wordNum) {
        char *word = words[wordNum];
        for (int row = 0; row < rowsCount; ++row) {

            for (int cell = 0; cell < colsCount; ++cell) {
                if (word[0] == matrix[row][cell].letter) {
                    checkAllDirections(word, matrix, cell, row, rowsCount, colsCount, wordNum);
                }
            }
        }
        if (!metaWords[wordNum].used) {
            printf("Slovo \'%s\' nenalezeno.\n", word);
            return 0;
        }
    }
    return 1;
}

void printAllUnusedPoints(int colsCount, int rowsCount, Point **matrix) {
    int counter = 0;
    int passed = 0;
    for (int row = 0; row < rowsCount; ++row) {
        for (int i = 0; i < colsCount; ++i) {
            if (!matrix[row][i].occupied && matrix[row][i].letter != '.') {
                if (counter == 60) {
                    printf("\n");
                    counter = 0;
                }
                if (!passed) passed = 1;
                printf("%c", matrix[row][i].letter);
                ++counter;
            }
        }
    }
    if (passed) printf("\n");
}

void checkRight(int cell, Meta *meta, char *word, Point **matrix, int row) {
    int passed = 1;
    for (int i = 1; i < meta->len; ++i) {
        if (word[i] != matrix[row][cell + i].letter) {
            passed = 0;
            break;
        }
    }

    if (passed) {
        meta->used = 1;
        for (int i = 0; i < meta->len; ++i) {
            matrix[row][cell + i].occupied = 1;
        }
    }
}

void checkLeft(int cell, Meta *meta, char *word, Point **matrix, int row) {
    int passed = 1;
    for (int i = 1; i < meta->len; ++i) {
        if (word[i] != matrix[row][cell - i].letter) {
            passed = 0;
            break;
        }
    }

    if (passed) {
        meta->used = 1;
        for (int i = 0; i < meta->len; ++i) {
            matrix[row][cell - i].occupied = 1;
        }
    }
}

void checkTop(int cell, Meta *meta, char *word, Point **matrix, int row) {
    int passed = 1;
    for (int i = 1; i < meta->len; ++i) {
        if (word[i] != matrix[row - i][cell].letter) {
            passed = 0;
            break;
        }
    }

    if (passed) {
        meta->used = 1;
        for (int i = 0; i < meta->len; ++i) {
            matrix[row - i][cell].occupied = 1;
        }
    }
}

void checkDown(int cell, Meta *meta, char *word, Point **matrix, int row) {
    int passed = 1;
    for (int i = 0; i < meta->len; ++i) {
        if (word[i] != matrix[row + i][cell].letter) {
            passed = 0;
            break;
        }
    }

    if (passed) {
        meta->used = 1;
        for (int i = 0; i < meta->len; ++i) {
            matrix[row + i][cell].occupied = 1;
        }
    }
}


void checkRightTop(int cell, Meta *meta, char *word, Point **matrix, int row) {
    int j = 0;
    int passed = 1;
    for (int i = 0; i < meta->len; ++i, ++j) {
        if (word[i] != matrix[row - i][cell + j].letter) {
            passed = 0;
            break;
        }
    }

    if (passed) {
        j = 0;
        meta->used = 1;
        for (int i = 0; i < meta->len; ++i, ++j) {
            matrix[row - i][cell + j].occupied = 1;
        }
    }
}

void checkLeftTop(int cell, Meta *meta, char *word, Point **matrix, int row) {
    int j = 0;
    int passed = 1;
    for (int i = 0; i < meta->len; ++i, ++j) {
        if (word[i] != matrix[row - i][cell - j].letter) {
            passed = 0;
            break;
        }
    }

    if (passed) {
        j = 0;
        meta->used = 1;
        for (int i = 0; i < meta->len; ++i, ++j) {
            matrix[row - i][cell - j].occupied = 1;
        }
    }
}

void checkLeftDown(int cell, Meta *meta, char *word, Point **matrix, int row) {
    int j = 0;
    int passed = 1;
    for (int i = 0; i < meta->len; ++i, --j) {
        if (word[i] != matrix[row + i][cell + j].letter) {
            passed = 0;
            break;
        }
    }

    if (passed) {
        j = 0;
        meta->used = 1;
        for (int i = 0; i < meta->len; ++i, --j) {
            matrix[row + i][cell + j].occupied = 1;
        }
    }
}

void checkRightDown(int cell, Meta *meta, char *word, Point **matrix, int row) {
    int j = 0;
    int passed = 1;
    for (int i = 0; i < meta->len; ++i, ++j) {
        if (word[i] != matrix[row + i][cell + j].letter) {
            passed = 0;
            break;
        }
    }

    if (passed) {
        j = 0;
        meta->used = 1;
        for (int i = 0; i < meta->len; ++i, ++j) {
            matrix[row + i][cell + j].occupied = 1;
        }
    }
}

void checkAllDirections(char *word, Point **matrix, int cell, int row, int rowsCount, int colsCount, int wordNum) {
    Meta *meta = &metaWords[wordNum];

    // projít dopředu
    if (cell + meta->len <= colsCount) {
        checkRight(cell, meta, word, matrix, row);
    }

    //projít dozadu
    if (cell - (meta->len - 1) >= 0) {
        checkLeft(cell, meta, word, matrix, row);
    }

    if (meta->len - 1 <= row) {
        //projit nahoru
        checkTop(cell, meta, word, matrix, row);

        //projít diagonálně / nahoru
        if (cell + meta->len <= colsCount) {
            checkRightTop(cell, meta, word, matrix, row);
        }

        //projít diagonálně \ nahoru
        if (cell - (meta->len - 1) >= 0) {
            checkLeftTop(cell, meta, word, matrix, row);
        }
    }

    if (meta->len <= rowsCount - row) {
        //projít dolu
        checkDown(cell, meta, word, matrix, row);

        //projít diagonálně / dolu
        if (cell - (meta->len - 1) >= 0) {
            checkLeftDown(cell, meta, word, matrix, row);
        }

        //projít diagonálně \ dolu
        if (cell + meta->len <= colsCount) {
            checkRightDown(cell, meta, word, matrix, row);
        }
    }
}
