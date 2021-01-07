#include <stdio.h>
#include <stdlib.h>

int input(void) {
    return rand();
}

int main() {
    int i = rand();
    int j = rand();

    for (int k = 0; k < 10; k++) {
        if ((i + j) % 2 == 0) {
            i += 2;
            j += 3;
        } else {
            i += 1;
            j += 7;
        }
    }

    printf("ij: %d, %d\n", i, j);

    return 0;
}
