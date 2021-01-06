#include <stdio.h>
#include <stdlib.h>

float t = 10.233;

int sum(int i, int j) {
    int d = rand();
    if (i % 2 == 0) {
        return i + j + d;
    } else {
        return i - j + d;
    }
}

int main() {
    int j = 0;
    t += 1;
    for (int i = 0; i < 100; i ++) {
        if (i % 3 == 0) {
            continue;
        }

        int c = sum(i, j);
        printf("c: %d\n", c);

        if (i == 8) {
            continue;
        }

        if (i % 2 == 0) {
            printf("even: %d\n", j);
        } else {
            j += 1;
            printf("odd\n");
        }
    }

    printf("%f\n", t);
}
