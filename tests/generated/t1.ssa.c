
/* Provide Declarations */
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <setjmp.h>
#include <limits.h>
#include <stdint.h>
#include <math.h>
#ifndef __cplusplus
typedef unsigned char bool;
#endif



/* Global Declarations */

/* Types Declarations */

/* Function definitions */

/* Types Definitions */

/* External Global Variable Declarations */

/* Function Declarations */
void foo(void);
int printf(const char*, ...);
int main(void);


/* Global Variable Definitions and Initialization */


/* Function Bodies */





void foo_2(void) {
  printf("x: %d\n", 9);
}

void foo_1(void) {
  printf("x: %d\n", 9);
}

int main(void) {
  foo_1();
  foo_2();
  return 0;
}
