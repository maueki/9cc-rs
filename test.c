#include <stdio.h>
#include <stdlib.h>

void foo() {
    printf("call foo Ok\n");
}

int sub(int a, int b) {
    printf("call sub a=%d, b=%d\n", a, b);
    return a-b;
}

void alloc4(int **p, int a, int b, int c, int d) {
    *p = malloc(sizeof(int) * 4);
    int *ap = *p;
    ap[0] = a;
    ap[1] = b;
    ap[2] = c;
    ap[3] = d;
}
