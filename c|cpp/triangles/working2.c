#include <stdio.h>
#include <math.h>
#include <float.h>

int error() {
    printf("Nespravny vstup.\n");
    return 1;
}

int equals(double one, double two) {
    return fabs(one - two) <= DBL_EPSILON * fmax(fabs(one), fabs(two)) * 10;
}

double getLength(double A1[], double B1[]) {
    return sqrt(pow((B1[0] - A1[0]), 2) + pow((B1[1] - A1[1]), 2));
}
const char points[3] = {'A', 'B', 'C'};

int loadPoints(double triangle[4][2]) {
    triangle[3][0] = 0;
    for (int i = 0; i < 3; i++) {
        printf("Bod %c:\n", points[i]);
        int result = scanf("%lf %lf", &triangle[i][0], &triangle[i][1]);
        if (result != 2) {
            return error();
        }
    }
    return 0;
}

int checkIfTriangleExist(double triangle[4][2]) {
    if (triangle[3][0] > 0) { //pasted sites length

        return !equals(triangle[0][0] + triangle[1][0], triangle[2][0]) &&
               triangle[0][0] + triangle[1][0] > triangle[2][0];
    } else {
        double *A = triangle[0];
        double *B = triangle[1];
        double *C = triangle[2];
        double one = (B[0] - A[0]) * (C[1] - A[1]);
        double two = (B[1] - A[1]) * (C[0] - A[0]);

        return fabs(one - two) > DBL_EPSILON * fmax(fabs(one), fabs(two)) * 10;
    }
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

int isSame(double triangle1[4][2], double triangle2[4][2]) {
    double a1, b1, c1, a2, b2, c2;
    if (triangle1[3][0] > 0) {
        a1 = triangle1[0][0];
        b1 = triangle1[1][0];
        c1 = triangle1[2][0];
    } else {
        double *A1 = triangle1[0];
        double *B1 = triangle1[1];
        double *C1 = triangle1[2];
        a1 = getLength(A1, B1);
        b1 = getLength(B1, C1);
        c1 = getLength(A1, C1);
    }

    if (triangle2[3][0] > 0) {
        a2 = triangle2[0][0];
        b2 = triangle2[1][0];
        c2 = triangle2[2][0];
    } else {
        double *A2 = triangle2[0];
        double *B2 = triangle2[1];
        double *C2 = triangle2[2];
        a2 = getLength(A2, B2);
        b2 = getLength(B2, C2);
        c2 = getLength(A2, C2);
    }

    if (areSidesSame(a1, b1, c1, a2, b2, c2)) {
        printf("Trojuhelniky jsou shodne.\n");
        return 0;
    }
    double circuit1 = a1 + b1 + c1;
    double circuit2 = a2 + b2 + c2;
    if (equals(circuit1, circuit2)) {
        printf("Trojuhelniky nejsou shodne, ale maji stejny obvod.\n");
    } else {
        if (circuit1 - circuit2 > DBL_EPSILON * circuit1) {
            printf("Trojuhelnik #1 ma vetsi obvod.\n");
        } else {
            printf("Trojuhelnik #2 ma vetsi obvod.\n");
        }
    }
    return 0;
}

int areLengthPositive(double triangle[4][2]) {
    if (triangle[3][0]) {
        return triangle[0][0] > 0 && triangle[1][0] > 0 && triangle[2][0] > 0;
    }
    return 1;
}

int main() {
    double triangle1[4][2], triangle2[4][2];

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
