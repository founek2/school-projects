#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define DEFAULT_ALOC 100

int error() {
    printf("Nespravny vstup.\n");
    return 1;
}

typedef struct {
    char value[31];
    int len;
    int used;
} Word;

typedef struct {
    char letter;
    int occupied;
} Point;

Point *reallocate(Point *array, int len) {
    return (Point *) realloc(array, len * sizeof(Point));
}

void addToMatrix(char value, int index, int row, Point **matrix) {
    Point point;
    point.letter = value;
    point.occupied = 0;
    matrix[row][index] = point;
}

Point **loadMatrix(int *rowsCount, int *colsCount) {
    Point **matrix = (Point **) malloc(DEFAULT_ALOC * sizeof(Point *));
    int allocated = DEFAULT_ALOC;
    *rowsCount = 0;
    *colsCount = 0;
    int allocatedRows = DEFAULT_ALOC;
    matrix[*rowsCount] = (Point *) malloc(allocatedRows * sizeof(Point));

    char letter;
    while (scanf("%c", &letter) == 1) {
        if ((letter >= 'a' && letter <= 'z') || letter == '.') {
            if (allocated - 1 == *colsCount) {
                allocated *= 2;
                matrix[0] = reallocate(matrix[0], allocated);
            }
            addToMatrix(letter, *colsCount, *rowsCount, matrix);
            (*colsCount)++;
        } else if (letter == '\n') {
            ++*rowsCount;
            break;
        } else
            return NULL;
    }

    //TODO chybí realloc pro řádky a sloupce
    int localColCounter = 0;
    while (scanf("%c", &letter) == 1) {
        if ((letter >= 'a' && letter <= 'z') || letter == '.') {
            if (allocatedRows == *rowsCount) {
                allocatedRows *= 2;
                matrix = (Point **) realloc(matrix, allocatedRows * sizeof(Point *));
            }
            if (localColCounter == 0) {
                matrix[*rowsCount] = (Point *) malloc(*colsCount * sizeof(Point));
            }
            if (localColCounter == *colsCount)
                return NULL;
            addToMatrix(letter, localColCounter, *rowsCount, matrix);
            localColCounter++;
        } else if (letter == '\n') {
            if (localColCounter == 0)
                break;
            if (localColCounter != *colsCount)
                return NULL;
            localColCounter = 0;
            ++*rowsCount;
        } else
            return NULL;

    }

    if (*rowsCount == 0 || *colsCount == 0)
        return NULL;

    return matrix;
}

Word createWord(char value[31]) {
    Word word;
    word.len = (int) strlen(value);
    word.used = 0;
    strcpy(word.value, value);
    return word;
}

Word *loadWords(int *wordsCount) {
    *wordsCount = 0;
    Word *words = (Word *) malloc(DEFAULT_ALOC * sizeof(Word));
    int allocated = DEFAULT_ALOC;
    char wordTmp[31];
    int result = 0;

    while ((result = scanf("%s", wordTmp)) && result == 1) {
        if (*wordsCount == allocated) {
            allocated += allocated / 2;
            words = (Word *) realloc(words, allocated * sizeof(Word));
        }

        words[(*wordsCount)++] = createWord(wordTmp);
    }
    if (*wordsCount == 0)
        return NULL;

    return words;
}

void checkAllDirections(Word * word, Point ** matrix, int cell, int row, int rowsCount, int colsCount) {
    if (word->value[0] == matrix[row][cell].letter) {
        // projít dopředu
        int passed = 1;
        int j = 0;
        if (cell + word->len <= colsCount) {
            for (int i = 0; i < word->len; ++i) {
                if (word->value[i] != matrix[row][cell + i].letter) {
                    passed = 0;
                    break;
                }
            }

            if (passed) {
                word->used = 1;
                for (int i = 0; i < word->len; ++i) {
                    matrix[row][cell + i].occupied = 1;
                }
                return;
            }
        }
        //projít dozadu
        if (cell - (word->len - 1) >= 0) {
            passed = 1;
            for (int i = 0; i < word->len; ++i) {
                if (word->value[i] != matrix[row][cell - i].letter) {
                    passed = 0;
                    break;
                }
            }

            if (passed) {
                word->used = 1;
                for (int i = 0; i < word->len; ++i) {
                    matrix[row][cell - i].occupied = 1;
                }
                return;
            }
        }

        if (word->len - 1 <= row) {
            //projit nahoru
            passed = 1;
            for (int i = 0; i < word->len; ++i) {
                if (word->value[i] != matrix[row - i][cell].letter) {
                    passed = 0;
                    break;
                }
            }

            if (passed) {
                word->used = 1;
                for (int i = 0; i < word->len; ++i) {
                    matrix[row - i][cell].occupied = 1;
                }
                return;
            }

            j = 0;
            //projít diagonálně / nahoru
            if (cell + word->len <= colsCount) {
                passed = 1;
                for (int i = 0; i < word->len; ++i, ++j) {
                    if (word->value[i] != matrix[row - i][cell + j].letter) {
                        passed = 0;
                        break;
                    }
                }

                if (passed) {
                    j = 0;
                    word->used = 1;
                    for (int i = 0; i < word->len; ++i, ++j) {
                        matrix[row - i][cell + j].occupied = 1;
                    }
                    return;
                }
            }

            //projít diagonálně \ nahoru
            if (cell - (word->len - 1) >= 0) {
                passed = 1;
                j = 0;
                for (int i = 0; i < word->len; ++i, ++j) {
                    if (word->value[i] != matrix[row - i][cell - j].letter) {
                        passed = 0;
                        break;
                    }
                }

                if (passed) {
                    j = 0;
                    word->used = 1;
                    for (int i = 0; i < word->len; ++i, ++j) {
                        matrix[row - i][cell - j].occupied = 1;
                    }
                    return;
                }
            }
        }

        if (word->len - 1 <= rowsCount - 1 - row) {
            //projít dolu
            passed = 1;
            for (int i = 0; i < word->len; ++i) {
                if (word->value[i] != matrix[row + i][cell].letter) {
                    passed = 0;
                    break;
                }
            }
            if (passed) {
                word->used = 1;
                for (int i = 0; i < word->len; ++i) {
                    matrix[row + i][cell].occupied = 1;
                }
                return;
            }

            //TODO ošetřit pro diagonály levé/pravé max
            if (cell - (word->len - 1) >= 0) {
                //projít diagonálně / dolu
                passed = 1;
                int j = 0;
                for (int i = 0; i < word->len; ++i, --j) {
                    if (word->value[i] != matrix[row + i][cell + j].letter) {
                        passed = 0;
                        break;
                    }
                }
                if (passed) {
                    j = 0;
                    word->used = 1;
                    for (int i = 0; i < word->len; ++i, --j) {
                        matrix[row + i][cell + j].occupied = 1;
                    }
                    return;
                }
            }

            //projít diagonálně \ dolu
            if (cell + word->len - 1 <= colsCount) {
                passed = 1;
                j = 0;
                for (int i = 0; i < word->len; ++i, ++j) {
                    if (word->value[i] != matrix[row + i][cell + j].letter) {
                        passed = 0;
                        break;
                    }
                }

                if (passed) {
                    j = 0;
                    word->used = 1;
                    for (int i = 0; i < word->len; ++i, ++j) {
                        matrix[row + i][cell + j].occupied = 1;
                    }
                    return;
                }
            }
        }
    }
}

int findAllWords(int colsCount, int rowsCount, Point ** matrix, int wordsCount, Word * words) {
    for (int wordNum = 0; wordNum < wordsCount; ++wordNum) {
        Word *word = &words[wordNum];
        for (int row = 0; row < rowsCount; ++row) {

            for (int cell = 0; cell < colsCount; ++cell) {
                if (matrix[row][cell].letter != '.') {
                    checkAllDirections(word, matrix, cell, row, rowsCount, colsCount);
                }
            }
        }
        if (!word->used) {
            printf("Slovo \'%s\' nenalezeno.\n", word->value);
            return 0;
        }
    }
    return 1;
}

void printAllUnusedPoints(int colsCount, int rowsCount, Point **matrix) {
    int counter = 0;
    for (int row = 0; row < rowsCount; ++row) {
        for (int i = 0; i < colsCount; ++i) {
            if (!matrix[row][i].occupied && matrix[row][i].letter != '.') {
                if (counter == 60) {
                    printf("\n");
                    counter = 0;
                }
                printf("%c", matrix[row][i].letter);
                ++counter;
            }
        }
    }

}

//TODO je potřeba u každého slova vědět jestli bylo nalezeno, a u bodu jestli byly použity aspoň jednou
int main() {
    printf("Zadejte osmismerku:\n");

    int colsCount, rowsCount;
    Point **matrix = loadMatrix(&rowsCount, &colsCount);
    if (matrix == NULL) {
        return error();
    }
    int wordCount;
    Word *words = loadWords(&wordCount);
    if (words == NULL)
        return error();

    //Jump * jumpTable = createJumpTables(wordCount, words);

    if (!findAllWords(colsCount, rowsCount, matrix, wordCount, words))
        return 0;

    printf("Vysledek:\n");
    printAllUnusedPoints(colsCount, rowsCount, matrix);

    return 0;
}
