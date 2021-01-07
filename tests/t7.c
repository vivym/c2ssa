#include <stdio.h>

int main() {
    int i = 0, j = 0;
    scanf("%d %d", &i, &j);

    int sum = 0;
    for (int n = 0; n < 10; n ++) {
        if (n % 2 == 0) {
            sum += i + j;
        } else {
            sum += i * j;
        }
        i += 1;
        j += 2;
    }

    printf("sum: %d\n", sum);
}
