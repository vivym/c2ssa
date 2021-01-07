#include <stdio.h>
#include <stdlib.h>

const int n = 10;

typedef struct {
    int x;
    int y;
} Point;

int inc(int a) {
    int r = rand() % a;
    return a + r;
}

int main() {
    Point p = { 10, 20 };

    for (int i = 0; i < n; i++) {
        if (i % 3 == 0) {
            continue;
        }

        switch (i % 4) {
        case 0:
            printf("mod: 0\n");
            break;
        case 1:
            p.y += i;
            printf("mod: 1\n");
            break;
        case 2:
            printf("mod: 1\n");
            break;
        default:
            printf("mod: 3\n");
            p.x += i;
            break;
        }

        if (i % 2 == 0) {
            printf("%d\n", inc(i));
        } else {
            p.x += 2;
        }

        if (i == 7) {
            break;
        }
    }

    printf("Point: %d, %d\n", p.x, p.y);
}
