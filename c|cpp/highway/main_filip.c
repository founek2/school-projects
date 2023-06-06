#include <printf.h>

int rec (int x )
{
    int i, j;
    int counter = 0;
    for ( i = 0; i < 2 * 30; i ++ )
        for ( j = 0; j < i; j ++ )
            counter++;
    return counter;
}


int main(){
    int r = rec ( -145 );
    printf("%d", r);
}