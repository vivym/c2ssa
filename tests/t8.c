#include <stdio.h>
#include <stdlib.h>

void print(int x) {
    printf("%d\n", x);
}

int input(void) {
    return rand();
}

int main() {
    srand(10);
    int i = input(), j = input();
    int c = i + 10 + j;

    if (i < 10) {
        i += 2;
        j += 3;
    } else {
        i = i + j;
        j = i + 10;
    }

    if (j > 100) {
        i += 2;
        j -= 3;
    } else {
        i = i + j;
        j = i + 10;
    }

    if (j > 100) {
        i += 2;
        j -= 3;
    } else {
        i = i + j;
        j = i + 10;
    }

    if (j > 100) {
        i += 2;
        j -= 3;
    } else {
        i = i + j;
        j = i + 10;
    }

    if (j > 100) {
        i += 2;
        j -= 3;
    } else {
        i = i + j;
        j = i + 10;
    }

    if (j > 100) {
        i += 2;
        j -= 3;
    } else {
        i = i + j;
        j = i + 10;
    }

    if (j > 100) {
        i += 2;
        j -= 3;
    } else {
        i = i + j;
        j = i + 10;
    }

    if (j > 100) {
        i += 2;
        j -= 3;
    } else {
        i = i + j;
        j = i + 10;
    }

    if (j > 100) {
        i += 2;
        j -= 3;
    } else {
        i = i + j;
        j = i + 10;
    }

    if (j > 100) {
        i += 2;
        j -= 3;
    } else {
        i = i + j;
        j = i + 10;
    }

    if (j > 100) {
        i += 2;
        j -= 3;
    } else {
        i = i + j;
        j = i + 10;
    }

    if (j > 100) {
        i += 2;
        j -= 3;
    } else {
        i = i + j;
        j = i + 10;
    }

    if (j > 100) {
        i += 2;
        j -= 3;
    } else {
        i = i + j;
        j = i + 10;
    }

    if (j > 100) {
        i += 2;
        j -= 3;
    } else {
        i = i + j;
        j = i + 10;
    }

    if (j > 100) {
        i += 2;
        j -= 3;
    } else {
        i = i + j;
        j = i + 10;
    }

    int d = 0;
    if ((i + j) < 100) {
        d = i + j;
        print(d);
    } else {
        d = i / j;
        print(d);
        print(i);
        print(j);
    }

    print(c);
    print(d);
    print(c + d);

    return 0;
}
