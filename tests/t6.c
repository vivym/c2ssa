#include <stdio.h>

void f8() {
    printf("f8\n");
}

void f6() {
    printf("f6\n");

    f8();
}

void f5() {
    printf("f5\n");

    f8();
}

void f7() {
    printf("f7\n");

    f5();
    f8();
}

void f4() {
    printf("f4\n");

    f5();
    f7();
}

void f3() {
    printf("f3\n");

    f5();
    f6();
}

void f2() {
    printf("f2\n");

    f4();
    f3();
}

void f1() {
    printf("f1\n");

    f2();
    f3();
}

int main() {
    f1();

    return 0;
}