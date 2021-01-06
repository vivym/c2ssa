#include <stdio.h>

void foo()
{
    int x=7;
    x=8;
    x=9;

    printf("x: %d\n", x);
}

int main()
{
    foo();
    foo();
}
