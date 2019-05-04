#include <stdio.h>

void foo() {
    printf("call foo Ok\n");
}

int sub(int a, int b) {
    printf("call sub a=%d, b=%d\n", a, b);
    return a-b;
}
