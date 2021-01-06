#include <stdio.h>

const int n = 10;

typedef struct {
    int x;
    int y;
} Point;

int inc(int a) {
    return a + 1;
}

int main() {
    Point p;

    for (int i = 0; i < n; i++) {
        if (i % 3 == 0) {
            continue;
        }

        switch (i % 4) {
        case 0:
            printf("mod: 0\n");
        case 1:
            p.y = i;
            printf("mod: 1\n");
        case 2:
            printf("mod: 1\n");
        default:
            printf("mod: 3\n");
            p.x = i;
        }

        if (i % 2 == 0) {
            printf("%d\n", inc(i));
        } else {
            i += 2;
        }

        if (i == 7) {
            break;
        }
    }

    printf("Point: %d, %d\n", p.x, p.y);
}
