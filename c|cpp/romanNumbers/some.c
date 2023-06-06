//
// Created by Martin Skalický on 14/12/2018.
//

#include <printf.h>

void drawLine(int count) {
    if (count <= 0)
        return;
    printf("*");
    return drawLine(count - 1);
}

void drawInnerLine(int heigh, int count) {
    if (heigh == 0)
        return;
    printf("x");
    drawLine(count - 2);
    printf("x\n");
    return drawInnerLine(heigh - 1, count);

}


int main() {
    drawInnerLine(10, 10);
}