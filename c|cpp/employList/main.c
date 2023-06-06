#ifndef __PROGTEST__

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

typedef struct TCar {
    struct TCar *m_Next;
    char *m_Model;
} TCAR;

typedef struct TEmployee {
    struct TEmployee *m_Next;
    struct TCar *m_Car;
    char *m_Name;
} TEMPLOYEE;

typedef struct TOffice {
    TEMPLOYEE *m_Emp;
    TCAR *m_Car;
} TOFFICE;

#endif /* __PROGTEST__ */

TOFFICE *initOffice() {
    TOFFICE *newOffice = (TOFFICE *) malloc(sizeof(struct TOffice));
    newOffice->m_Car = NULL;
    newOffice->m_Emp = NULL;
    return newOffice;
}

TEMPLOYEE * createEmploy(const char *name){
    TEMPLOYEE *newEmploy = (TEMPLOYEE *) malloc(sizeof(struct TEmployee));
    char *nameCp = (char *) malloc(strlen(name) + 1);
    strcpy(nameCp, name);
    newEmploy->m_Name = nameCp;
    newEmploy->m_Next = NULL;
    newEmploy->m_Car = NULL;
    return newEmploy;
}

void addEmployee(TOFFICE *office,
                 const char *name) {
    TEMPLOYEE * newEmploy = createEmploy(name);

    if (office->m_Emp != NULL) {
        TEMPLOYEE *firstEmploy = office->m_Emp;
        newEmploy->m_Next = firstEmploy;
        office->m_Emp = newEmploy;
    } else {
        office->m_Emp = newEmploy;
    }
}

TCAR * createCar(const char * model){
    TCAR * newCar = (TCAR *) malloc(sizeof(TCAR));
    newCar->m_Next = NULL;
    char *nameCp = (char *) malloc(strlen(model) + 1);
    strcpy(nameCp, model);
    newCar->m_Model = nameCp;
    return newCar;
}

void addCar(TOFFICE *office,
            const char *model) {
    TCAR *newCar = createCar(model);

    if (office->m_Car != NULL) {
        TCAR *lastCar = office->m_Car;

        newCar->m_Next = lastCar;
        office->m_Car = newCar;
    } else {
        office->m_Car = newCar;
    }
}

TOFFICE *cloneOffice(TOFFICE *office) {
    TOFFICE *newOffice = initOffice();

    if (office->m_Emp != NULL) {
        TEMPLOYEE *firstEmploy = office->m_Emp;

        TEMPLOYEE* lastEmploy = NULL;
        while (firstEmploy->m_Next != NULL) {
            const char * name = firstEmploy->m_Name;
            TEMPLOYEE *newEmploy = createEmploy(name);

            if (newOffice->m_Emp == NULL) {
                newOffice->m_Emp = newEmploy;
            } else {
                lastEmploy->m_Next = newEmploy;
            }
            lastEmploy = newEmploy;

            firstEmploy = firstEmploy->m_Next;
        };

        const char * name = firstEmploy->m_Name;
        TEMPLOYEE *newEmploy = createEmploy(name);

        if (newOffice->m_Emp == NULL) {
            newOffice->m_Emp = newEmploy;
        } else {
            lastEmploy->m_Next = newEmploy;
        }
    }

    if (office->m_Car != NULL) {
        TCAR *firstCar = office->m_Car;

        TCAR *lastCar = NULL;
        while (firstCar->m_Next != NULL) {
            const char * model = firstCar->m_Model;
            TCAR *newCar = createCar(model);

            if (newOffice->m_Car == NULL){
                newOffice->m_Car = newCar;
            } else {
                lastCar->m_Next = newCar;
            }

            lastCar = newCar;
            firstCar = firstCar->m_Next;
        }

        const char * model = firstCar->m_Model;
        TCAR *newCar = createCar(model);

        if (newOffice->m_Car == NULL){
            newOffice->m_Car = newCar;
        } else {
            lastCar->m_Next = newCar;
        }

    }

    //TODO procházet oba seznamy aut a hledat kde je ukazatel na něj v employ seznamu -> když jsou adresy stejné,
    // tak přidat i v novém adresu na nové auto

    if (office->m_Emp != NULL && office->m_Car != NULL) {

        TCAR *oldCar = office->m_Car;
        TCAR *newCar = newOffice->m_Car;

        while (oldCar->m_Next != NULL) {
            TEMPLOYEE *oldEmploy = office->m_Emp;
            TEMPLOYEE *newEmploy = newOffice->m_Emp;
            while (oldEmploy->m_Next != NULL) {
                if (oldEmploy->m_Car == oldCar) {
                    newEmploy->m_Car = newCar;
                }
                oldEmploy = oldEmploy->m_Next;
                newEmploy = newEmploy->m_Next;
            }
            if (oldEmploy->m_Car == oldCar) {
                newEmploy->m_Car = newCar;
            }

            oldCar = oldCar->m_Next;
            newCar = newCar->m_Next;
        }

        TEMPLOYEE *oldEmploy = office->m_Emp;
        TEMPLOYEE *newEmploy = newOffice->m_Emp;
        while (oldEmploy->m_Next != NULL) {
            if (oldEmploy->m_Car == oldCar) {
                newEmploy->m_Car = newCar;
            }
            oldEmploy = oldEmploy->m_Next;
            newEmploy = newEmploy->m_Next;
        }
        if (oldEmploy->m_Car == oldCar) {
            newEmploy->m_Car = newCar;
        }
    }

    return newOffice;
}

void freeOffice(TOFFICE *office) {
    if (office->m_Emp != NULL) {
        TEMPLOYEE *lastEmpl = office->m_Emp;

        while (lastEmpl->m_Next != NULL) {
            TEMPLOYEE *tmp = lastEmpl->m_Next;
            free(lastEmpl->m_Name);
            free(lastEmpl);
            lastEmpl = tmp;
        }
        free(lastEmpl->m_Name);
        free(lastEmpl);
    }

    if (office->m_Car != NULL) {
        TCAR *lastCar = office->m_Car;

        while (lastCar->m_Next != NULL) {
            TCAR *tmp = lastCar->m_Next;
            free(lastCar->m_Model);
            free(lastCar);
            lastCar = tmp;
        }
        free(lastCar->m_Model);
        free(lastCar);
    }
    free(office);
}

#ifndef __PROGTEST__

int main(int argc, char *argv[]) {
    TOFFICE *a, *b;
    char tmp[100];

    assert (sizeof(TOFFICE) == 2 * sizeof(void *));
    assert (sizeof(TEMPLOYEE) == 3 * sizeof(void *));
    assert (sizeof(TCAR) == 2 * sizeof(void *));
    a = initOffice();
    addEmployee(a, "Peter");
    addEmployee(a, "John");
    addEmployee(a, "Joe");
    addEmployee(a, "Maria");
    addCar(a, "Skoda Octavia");
    addCar(a, "VW Golf");
    a->m_Emp->m_Car = a->m_Car;
    a->m_Emp->m_Next->m_Next->m_Car = a->m_Car->m_Next;
    a->m_Emp->m_Next->m_Next->m_Next->m_Car = a->m_Car;

    assert(a->m_Emp);
    assert(!strcmp(a->m_Emp->m_Name, "Maria"));
    assert(a->m_Emp->m_Car == a->m_Car);
    assert (a->m_Emp
            && !strcmp(a->m_Emp->m_Name, "Maria")
            && a->m_Emp->m_Car == a->m_Car);
    assert (a->m_Emp->m_Next
            && !strcmp(a->m_Emp->m_Next->m_Name, "Joe")
            && a->m_Emp->m_Next->m_Car == NULL);
    assert (a->m_Emp->m_Next->m_Next
            && !strcmp(a->m_Emp->m_Next->m_Next->m_Name, "John")
            && a->m_Emp->m_Next->m_Next->m_Car == a->m_Car->m_Next);
    assert (a->m_Emp->m_Next->m_Next->m_Next
            && !strcmp(a->m_Emp->m_Next->m_Next->m_Next->m_Name, "Peter")
            && a->m_Emp->m_Next->m_Next->m_Next->m_Car == a->m_Car);
    assert (a->m_Emp->m_Next->m_Next->m_Next->m_Next == NULL);
    assert (a->m_Car
            && !strcmp(a->m_Car->m_Model, "VW Golf"));
    assert (a->m_Car->m_Next
            && !strcmp(a->m_Car->m_Next->m_Model, "Skoda Octavia"));
    assert (a->m_Car->m_Next->m_Next == NULL);
    b = cloneOffice(a);
    strncpy (tmp, "Moe", sizeof(tmp));
    addEmployee(a, tmp);
    strncpy (tmp, "Victoria", sizeof(tmp));
    addEmployee(a, tmp);
    strncpy (tmp, "Peter", sizeof(tmp));
    addEmployee(a, tmp);
    strncpy (tmp, "Citroen C4", sizeof(tmp));
    addCar(a, tmp);
    b->m_Emp->m_Next->m_Next->m_Next->m_Car = b->m_Car->m_Next->m_Next;
    assert (a->m_Emp
            && !strcmp(a->m_Emp->m_Name, "Peter")
            && a->m_Emp->m_Car == NULL);
    assert (a->m_Emp->m_Next
            && !strcmp(a->m_Emp->m_Next->m_Name, "Victoria")
            && a->m_Emp->m_Next->m_Car == NULL);
    assert (a->m_Emp->m_Next->m_Next
            && !strcmp(a->m_Emp->m_Next->m_Next->m_Name, "Moe")
            && a->m_Emp->m_Next->m_Next->m_Car == NULL);
    assert (a->m_Emp->m_Next->m_Next->m_Next
            && !strcmp(a->m_Emp->m_Next->m_Next->m_Next->m_Name, "Maria")
            && a->m_Emp->m_Next->m_Next->m_Next->m_Car == a->m_Car->m_Next);
    assert (a->m_Emp->m_Next->m_Next->m_Next->m_Next
            && !strcmp(a->m_Emp->m_Next->m_Next->m_Next->m_Next->m_Name, "Joe")
            && a->m_Emp->m_Next->m_Next->m_Next->m_Next->m_Car == NULL);
    assert (a->m_Emp->m_Next->m_Next->m_Next->m_Next->m_Next
            && !strcmp(a->m_Emp->m_Next->m_Next->m_Next->m_Next->m_Next->m_Name, "John")
            && a->m_Emp->m_Next->m_Next->m_Next->m_Next->m_Next->m_Car == a->m_Car->m_Next->m_Next);
    assert (a->m_Emp->m_Next->m_Next->m_Next->m_Next->m_Next->m_Next
            && !strcmp(a->m_Emp->m_Next->m_Next->m_Next->m_Next->m_Next->m_Next->m_Name, "Peter")
            && a->m_Emp->m_Next->m_Next->m_Next->m_Next->m_Next->m_Next->m_Car == a->m_Car->m_Next);
    assert (a->m_Emp->m_Next->m_Next->m_Next->m_Next->m_Next->m_Next->m_Next == NULL);
    assert (a->m_Car
            && !strcmp(a->m_Car->m_Model, "Citroen C4"));
    assert (a->m_Car->m_Next
            && !strcmp(a->m_Car->m_Next->m_Model, "VW Golf"));
    assert (a->m_Car->m_Next->m_Next
            && !strcmp(a->m_Car->m_Next->m_Next->m_Model, "Skoda Octavia"));
    assert (a->m_Car->m_Next->m_Next->m_Next == NULL);
    assert (b->m_Emp
            && !strcmp(b->m_Emp->m_Name, "Maria")
            && b->m_Emp->m_Car == b->m_Car);
    assert (b->m_Emp->m_Next
            && !strcmp(b->m_Emp->m_Next->m_Name, "Joe")
            && b->m_Emp->m_Next->m_Car == NULL);
    assert (b->m_Emp->m_Next->m_Next
            && !strcmp(b->m_Emp->m_Next->m_Next->m_Name, "John")
            && b->m_Emp->m_Next->m_Next->m_Car == b->m_Car->m_Next);
    assert (b->m_Emp->m_Next->m_Next->m_Next
            && !strcmp(b->m_Emp->m_Next->m_Next->m_Next->m_Name, "Peter")
            && b->m_Emp->m_Next->m_Next->m_Next->m_Car == NULL);
    assert (b->m_Emp->m_Next->m_Next->m_Next->m_Next == NULL);
    assert (b->m_Car
            && !strcmp(b->m_Car->m_Model, "VW Golf"));
    assert (b->m_Car->m_Next
            && !strcmp(b->m_Car->m_Next->m_Model, "Skoda Octavia"));
    assert (b->m_Car->m_Next->m_Next == NULL);
    freeOffice(a);
    freeOffice(b);
    return 0;
}

#endif /* __PROGTEST__ */
