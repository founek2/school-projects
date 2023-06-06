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

TEMPLOYEE * createEmployee(const char * name){
    TEMPLOYEE *NewEmployee = (TEMPLOYEE *) malloc(sizeof(TEMPLOYEE));
    char *NewName = (char *) malloc(sizeof(*name));
    strcpy(NewName, name); //kopiruje obsah name do NewName
    NewEmployee->m_Name = NewName;
    NewEmployee->m_Car = NULL;
    NewEmployee->m_Next = NULL;

    return NewEmployee;
}

TCAR * createCar(const char * model){
    TCAR *NewCar = (TCAR *) malloc(sizeof(TCAR));
    char *NewModel = (char *) malloc(sizeof(*model));
    strcpy(NewModel,model);
    NewCar -> m_Model = NewModel;
    NewCar -> m_Next = NULL;

    return NewCar;
}

TOFFICE *initOffice() {
    TOFFICE *NewOffice = (TOFFICE *) malloc(sizeof(TOFFICE));
    NewOffice->m_Emp = NULL; //ukazatel na strukturu, hodnota (vlastnost) jejiz hodnotu nastavuju
    NewOffice->m_Car = NULL;
    return NewOffice;
}

void addEmployee(TOFFICE *office,
                 const char *name) {

    TEMPLOYEE * NewEmployee = createEmployee(name);

    if(office -> m_Emp == NULL){
        office -> m_Emp = NewEmployee; //samotna promenna bez hvezdicky obsahuje adresu
        //fuknce create Employee zarucuje ze NewEmployee->m_Next=NULL;
    }
    else{
        NewEmployee->m_Next = office->m_Emp; //oba ukazuji na stareho zamestnance
        office -> m_Emp = NewEmployee; //presmerovani kam ukazuje office
    }
}

void addCar(TOFFICE *office,
            const char *model) {

    TCAR * NewCar = createCar(model);

    if(office -> m_Car == NULL){
        office -> m_Car = NewCar;
    }
    else{
        NewCar -> m_Next = office -> m_Car;
        office -> m_Car = NewCar;
    }
}

TOFFICE *cloneOffice(TOFFICE *office) {
    TOFFICE * NewOffice = initOffice(); //tvorba nove kancelare

    // 1. kopirovani zamestnancu
    TEMPLOYEE * LatestEmployee = office -> m_Emp;
    TEMPLOYEE * tmp = NULL; //docasna promenna na tvoreni odkazu
    while (LatestEmployee != NULL){ //cyklus kopirovani zamestnancu
        TEMPLOYEE * CloneEmployee = createEmployee(LatestEmployee -> m_Name);

        if (tmp != NULL){
            tmp -> m_Next = CloneEmployee;
        } else {
            NewOffice -> m_Emp = CloneEmployee;
        }
        tmp = CloneEmployee;
        LatestEmployee = LatestEmployee -> m_Next;
    }

    // 2. kopirovani aut
    TCAR * LatestCar = office -> m_Car;
    TCAR * tmpc = NULL;
    while(LatestCar != NULL){
        TCAR * CloneCar = createCar(LatestCar -> m_Model);
        if (tmpc != NULL){
            tmpc -> m_Next = CloneCar;
        } else {
            NewOffice ->m_Car = CloneCar;
        }
        tmpc = CloneCar;
        LatestCar = LatestCar -> m_Next;
    }



    // 3. tvorba vazeb
    TEMPLOYEE * FindEmployee = office -> m_Emp; //zamestnanci
    TCAR * FindCar = office -> m_Car; //auta
     //nove:

    TCAR * MyCar = NewOffice -> m_Car;
    TEMPLOYEE * MyEmployee = NewOffice -> m_Emp;
    while(FindCar != NULL){
        while(FindEmployee != NULL){

            //pokud existuje vazba mezi autem a zamestnancem, do novych (clone) se udelaji vazby
            if(FindEmployee -> m_Car == FindCar){
                MyEmployee -> m_Car = MyCar;
            }
            FindEmployee = FindEmployee -> m_Next;
            MyEmployee = MyEmployee -> m_Next;
        }
        FindEmployee = office -> m_Emp;
        MyEmployee = NewOffice -> m_Emp;
        FindCar = FindCar -> m_Next;
        MyCar = MyCar -> m_Next;
    }

    return NewOffice;
}

void freeOffice(TOFFICE *office) {
   /* while(office ->m_Emp != NULL){
        free(office -> m_Emp -> m_Name);
        free(office -> m_Emp);
        office -> m_Emp = office -> m_Emp ->m_Next;
    }

    while(office ->m_Car != NULL){
        free(office -> m_Car -> m_Model);
        free(office -> m_Car);
        office -> m_Car = office -> m_Car ->m_Next;
    }
*/
    while(office ->m_Car != NULL && office ->m_Emp != NULL){
        if (office ->m_Emp != NULL){
            free(office -> m_Emp -> m_Name);
            free(office -> m_Emp);}
        if(office ->m_Car != NULL){
            free(office -> m_Car -> m_Model);
            free(office -> m_Car);}
        free(office);
        if (office ->m_Car != NULL)
            office -> m_Car = office -> m_Car ->m_Next;
        if (office ->m_Emp != NULL)
            office -> m_Emp = office -> m_Emp ->m_Next;
    }

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
    strncpy(tmp, "Moe", sizeof(tmp));
    addEmployee(a, tmp);
    strncpy(tmp, "Victoria", sizeof(tmp));
    addEmployee(a, tmp);
    strncpy(tmp, "Peter", sizeof(tmp));
    addEmployee(a, tmp);
    strncpy(tmp, "Citroen C4", sizeof(tmp));
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
    assert (a->m_Emp->m_Next->m_Next->m_Next);
    assert (!strcmp(a->m_Emp->m_Next->m_Next->m_Next->m_Name, "Maria"));
    assert( a->m_Emp->m_Next->m_Next->m_Next->m_Car == a->m_Car->m_Next);
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
