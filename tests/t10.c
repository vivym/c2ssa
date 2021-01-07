#include <stdio.h>

int foo3() {
    printf("foo1 called\n");
    return 0;
}

int foo2() {
    printf("foo1 called\n");
    return 0;
}

int foo1() {
    printf("foo1 called\n");

    for (int i = 0; i < 10;i ++) {
        foo3();
    }
    return 0;
}

int main() {
    for (int i = 0; i < 4; i ++) {
        foo1();
        foo2();
    }

    return 0;
}
