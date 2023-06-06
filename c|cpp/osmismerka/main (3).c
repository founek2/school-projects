#include <stdio.h>
#include <stdlib.h>

typedef struct {
    char *words;
    int len;
    int nalezen;
} pole_struct;

char **allocate(int row, int col) {

    char **grid;
    grid = (char **) malloc(row * sizeof(char *));
    for (int i = 0; i < row; i++)
        grid[i] = (char *) malloc(col * sizeof(char));
    return grid;
}

char grid(char **tajenkoMapa, char **p, int *rows, int *cols) {

    char pismeno = 0;
    int act = 0;
    int count = 0;
    int count2 = 0;
    int limit2 = 0;
    int counter = 0;
    int counter2 = 0;
    int nejakyVysledek = 0;
    pole_struct *data;
    int sken;
    char lett;
    int slovJe = 0;
    int w = 0; //znaci, kolikate slovo se hleda
    int x[] = {-1, -1, 0, 0, 1, 1, -1, 1};
    int y[] = {0, 1, -1, 1, -1, 0, -1, 1};
    int k = 0;
    int nalezeno = 0;
    int prvniHotovo = 0;
    int countBasic = 0;
    int odradkovaniFix = 0;
    int wordLimit = 50;
    int letterLimit = 50;
    int skenko = 0;
    int limitFor1D = 50;
    int limitFor2D = 50;

    tajenkoMapa = (char **) malloc(limitFor1D * sizeof(char *));
    p = (char **) malloc(limitFor1D * sizeof(char *));

    for (int i = 0; i < limitFor2D; i++) {
        p[i] = (char *) malloc(limitFor2D * sizeof(char));
        tajenkoMapa[i] = (char *) malloc(limitFor2D * sizeof(char));
    }

    if (p == NULL)
        exit(1);

    if (p[count] == 0)
        exit(1);

    while (1) {
        if (count >= limitFor1D - 2) {
            limitFor1D *= 2;
            tajenkoMapa = (char **) realloc(tajenkoMapa, limitFor1D * sizeof(char *));
            p = (char **) realloc(p, limitFor1D * sizeof(char *));
            for (int i = count; i < limitFor1D; i++) {
                p[i] = (char *) realloc(p[i], limitFor1D * sizeof(char));
                tajenkoMapa[i] = (char *) realloc(tajenkoMapa[i], limitFor1D * sizeof(char));

            }
        }
        if (p == NULL)
            exit(1);

        if (p[count] == 0)
            exit(1);

        limit2 = count2;
        count2 = 0;


        while ((skenko = scanf("%c", &pismeno)) != EOF) {

            if (odradkovaniFix == 0 && pismeno == '\n') {
                printf("Nespravny vstup.\n");
                return 0;
            }
            odradkovaniFix = 1;

            if (((int) pismeno == 46) || ((int) pismeno == 10) ||
                ((((int) pismeno) >= 97) && (((int) pismeno) <= 122))) {


                if ((pismeno == '\n') && (act == 1)) {
                    *rows = count;
                    *cols = limit2;
                    if ((count < 1 || count2 < 1) && pismeno != '\n') {
                        printf("Nespravny vstup.\n");
                        return 0;
                    }
                    goto WORDS;
                }


                act = 0;
                if (limit2 <= count2) {
                    limit2++;
                }
                if (limit2 >= limitFor2D - 1 || count2 >= limitFor2D - 1) {
                    limitFor2D *= 2;
                    for (int i = 0; i < count; ++i) {
                        p[i] = (char *) realloc(p[i], (limitFor2D + 1) * sizeof(char));
                        tajenkoMapa[i] = (char *) realloc(tajenkoMapa[i], (limitFor2D + 1) * sizeof(char));
                    }

                }

                p[count][count2] = pismeno;
                if (pismeno != '\n') {
                    tajenkoMapa[count][count2] = 'k';
                }

                if (p[count][count2] == '.') {
                    tajenkoMapa[count][count2] = '.';
                }

                count2++;
                if ((prvniHotovo == 1 && count2 > countBasic)) {
                    printf("Nespravny vstup.\n");
                    return 0;
                }

                if (pismeno == '\n') {
                    act = 1;
                    if (count2 != countBasic && prvniHotovo == 1) {
                        printf("Nespravny vstup.\n");
                        return 0;
                    }
                    prvniHotovo = 1;
                    countBasic = count2;


                    break;
                }

            } else {
                printf("Nespravny vstup.\n");
                return 0;
            }
        }
        if (skenko == EOF) {
            printf("Nespravny vstup.\n");
            return 0;
        }
        count++;
    }
    WORDS:

    if (skenko == EOF) {
        goto TAJENKA;
    }

    data = (pole_struct *) malloc(wordLimit * sizeof(pole_struct));

    for (int l = 0; l < letterLimit; ++l) {
        data[l].words = (char *) malloc(letterLimit * sizeof(char));
    }

    while (1) {
        data[counter].len = 0;
        data[counter].words = (char *) malloc(letterLimit * sizeof(char));
        if (counter + 1 == wordLimit) {
            wordLimit *= 2;
            data = (pole_struct *) realloc(data, wordLimit * sizeof(pole_struct));
            for (int i = 0; i < wordLimit; i++) {
                data[i].words = (char *) realloc(data[i].words, letterLimit * sizeof(char));
            }
        }


        while ((sken = scanf("%c", &lett)) == 1) {
            if (lett == '\n') {
                break;
            }

            data[counter].words[counter2] = lett;
            data[counter].len++;

            counter2++;
            if (counter2 >= letterLimit) {
                letterLimit = letterLimit * 2;
                data[counter].words = (char *) realloc(data[counter].words, letterLimit * sizeof(char));
            }

            if (sken == EOF) {
                // printf("len = %d\n", data[w].len);
                goto PROGRAM;
            }

        }
        // printf("len = %d\n", data[w].len);
        if (sken == EOF) {

            goto PROGRAM;
        }

        counter2 = 0;
        counter++;
        slovJe++;
    }

    PROGRAM:

// Legend for me < 'cuz my code is really awesome.. yeah..
//p[][] <-- muj grid
//tajenkoMapa[][] <-- mapa prectenuch hodnot
//data[].word[] <-- jednotliva slova
//data[].len <-- delka techto slov
// i < count <-- precteni radku platnych
// j < limit2-1 <-- platne sloupecky
/*
for(int i = 0; i < count; i++){
    for (int j = 0; j < limit2; ++j) {
        printf("%c",p[i][j]);
    }
    printf("\n");
}

    for(int i = 0; i < count; i++){
        for (int j = 0; j < limit2; ++j) {
            printf("%c",tajenkoMapa[i][j]);
        }
        printf("\n");
    }
*/
    AGAIN:

    nalezeno = 0;
    for (int i = 0; i < count; i++) {
        for (int j = 0; j < limit2; j++) {
            for (int s = 0; s < 1; s++) {

                if (data[w].words[0] != p[i][j]) {
                    break;
                }
                for (int smer = 0; smer < 8; smer++) {

                    int rs = i + x[smer], cs = j + y[smer];


                    for (k = 1; k < data[w].len; ++k) {
                        if (rs < 0 || rs >= count || cs < 0 || cs >= limit2)
                            break;

                        if (p[rs][cs] != data[w].words[k])
                            break;

                        rs += x[smer], cs += y[smer];

                    }
                    if (k == data[w].len) {
                        int tajR = i;
                        int tajC = j;
                        for (int o = 0; o <= data[w].len; ++o) {
                            tajenkoMapa[tajR][tajC] = '1';
                            nalezeno++;
                            tajR += x[smer], tajC += y[smer];


                        }

                    }

                }

            }

        }

    }
    // printf("%d < %d \n", nalezeno, data[w].len);
    if (nalezeno < data[w].len) {
        printf("Slovo '%s' nenalezeno.\n", data[w].words);
        return -1;
    }

    if (w == slovJe) {
        goto TAJENKA;
    }
    w++;
    goto AGAIN;

    TAJENKA:

    printf("Vysledek:\n");
    int pocetVusledku = 0;
    for (int i = 0; i < count; i++) {
        for (int j = 0; j < limit2 - 1; j++) {
            if (tajenkoMapa[i][j] == 'k') {
                printf("%c", p[i][j]);
                pocetVusledku++;
                nejakyVysledek = 1;
                if (pocetVusledku >= 60) {
                    pocetVusledku = 0;
                    printf("\n");
                }
            }
        }
    }
    if (nejakyVysledek == 1) {
        printf("\n");
    }
    return 1;
}

int main(void) {
    char **p;
    char **tajenkoMapa;
    int count = 0;
    int limit2 = 0;
    printf("Zadejte osmismerku:\n");
    p = allocate(100, 100);
    tajenkoMapa = allocate(100, 100);
    grid(tajenkoMapa, p, &count, &limit2);

    return 0;
}