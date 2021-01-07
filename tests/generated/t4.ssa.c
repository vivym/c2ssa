
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
int c2ssa_main_call;
int c2ssa_main_call1;
int c2ssa_main_call16_2e_2;
int c2ssa_main_call16_2e_4;

/* Types Declarations */

/* Function definitions */

/* Types Definitions */

/* External Global Variable Declarations */

/* Function Declarations */
int inc(int);
int main(void);
int rand(void);
int printf(const char*, ...);
int puts(const char*);


/* Global Variable Definitions and Initialization */
const int n = 10;


/* Function Bodies */





int inc_2(int c2ssa_a) {
  return (c2ssa_a + 1);
}

int inc_1(int c2ssa_a) {
  return (c2ssa_a + 1);
}

int main(void) {
  c2ssa_main_call = rand();
  c2ssa_main_call1 = rand();
  puts("mod: 1");
  puts("mod: 1");
  puts("mod: 3");
  puts("mod: 1");
  puts("mod: 3");
  c2ssa_main_call16_2e_2 = inc_1(2);
  printf("%d\n", c2ssa_main_call16_2e_2);
  puts("mod: 0");
  puts("mod: 1");
  puts("mod: 1");
  puts("mod: 3");
  c2ssa_main_call16_2e_4 = inc_2(4);
  printf("%d\n", c2ssa_main_call16_2e_4);
  puts("mod: 1");
  puts("mod: 1");
  puts("mod: 3");
  puts("mod: 3");
  printf("Point: %d, %d\n", (c2ssa_main_call + 19), (c2ssa_main_call1 + 16));
  return 0;
}
