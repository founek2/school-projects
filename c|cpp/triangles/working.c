#include <stdio.h>
#include <math.h>
#include <float.h>

int error() {
    printf("Nespravny vstup.\n");
    return 1;
}

const char points[3] = {'A', 'B', 'C'};

int equals(double one, double two) {
    return fabs(one - two) < DBL_EPSILON * one;
}

int loadPoints(double triangle[3][2]) {
    for (int i = 0; i < 3; i++) {
        printf("Bod %c:\n", points[i]);
        int result = scanf("%lf %lf", &triangle[i][0], &triangle[i][1]);
        if (result != 2) {
            return error();
        }
    }
    return 0;
}

int checkIfTriangleExist(double triangle[3][2]) {
    double *A = triangle[0];
    double *B = triangle[1];
    double *C = triangle[2];

    double linear = (A[1] - B[1]) * C[0] + (B[0] - A[0]) * C[1] + (A[0] * B[1] - B[0] * A[1]);

    if (fabs(linear) < 0.00001) {
        return 0; // is not triangle
    }
    return 1; //is triangle
}

int areSame(double a1, double b1, double c1, double a2, double b2, double c2) {
    if (equals(a1, a2)) {
        if (equals(b1, b2)) {
            if (equals(c1, c2)) {
                return 1;
            }
        } else if (equals(b1, c2)) {
            if (equals(c1, b2)) {
                return 1;
            }
        }
    }
    return 0;
}

int areSidesSame(double a1, double b1, double c1, double a2, double b2, double c2) {
    return areSame(a1, b1, c1, a2, b2, c2) || areSame(b1, a1, c1, a2, b2, c2) || areSame(c1, a1, b1, a2, b2, c2);
}

int isSame(double triangle1[3][2], double triangle2[3][2]) {
    double *A1 = triangle1[0];
    double *B1 = triangle1[1];
    double *C1 = triangle1[2];
    double a1 = sqrt(pow((B1[0] - A1[0]), 2) + pow((B1[1] - A1[1]), 2));
    double b1 = sqrt(pow((C1[0] - B1[0]), 2) + pow((C1[1] - B1[1]), 2));
    double c1 = sqrt(pow((C1[0] - A1[0]), 2) + pow((C1[1] - A1[1]), 2));

    double *A2 = triangle2[0];
    double *B2 = triangle2[1];
    double *C2 = triangle2[2];
    double a2 = sqrt(pow((B2[0] - A2[0]), 2) + pow((B2[1] - A2[1]), 2));
    double b2 = sqrt(pow((C2[0] - B2[0]), 2) + pow((C2[1] - B2[1]), 2));
    double c2 = sqrt(pow((C2[0] - A2[0]), 2) + pow((C2[1] - A2[1]), 2));

    // printf("délky stran> %fl, %fl, %fl\n", a1, b1, c1);
    // printf("délky stran> %fl, %fl, %fl\n", a2, b2, c2);

    if (areSidesSame(a1, b1, c1, a2, b2, c2)) {
        printf("Trojuhelniky jsou shodne.\n");
        return 0;
    }
    double circuit1 = a1 + b1 + c1;
    double circuit2 = a2 + b2 + c2;
    if (equals(circuit1, circuit2)) {
        printf("Trojuhelniky nejsou shodne, ale maji stejny obvod.\n");
    } else {
        if (circuit1 > circuit2) {
            printf("Trojuhelnik #1 ma vetsi obvod.\n");
        } else {
            printf("Trojuhelnik #2 ma vetsi obvod.\n");
        }
    }
    return 0;
}

int main() {
    double triangle1[3][2], triangle2[3][2];

    printf("Trojuhelnik #1:\n");
    if (loadPoints(triangle1) == 1) {
        return 1;
    }

    if (!checkIfTriangleExist(triangle1)) {
        printf("Body netvori trojuhelnik.\n");
        return 1;
    }

    printf("Trojuhelnik #2:\n");
    if (loadPoints(triangle2) == 1) {
        return 1;
    }

    if (!checkIfTriangleExist(triangle2)) {
        printf("Body netvori trojuhelnik.\n");
        return 1;
    }

    return isSame(triangle1, triangle2);


}
