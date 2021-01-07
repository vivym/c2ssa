
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
int c2ssa_inc_2_call;
int c2ssa_inc_1_call;
int c2ssa_main_call12_2e_2;
int c2ssa_main_call12_2e_4;

/* Types Declarations */

/* Function definitions */

/* Types Definitions */

/* External Global Variable Declarations */

/* Function Declarations */
int inc(int);
int rand(void);
int main(void);
int printf(const char*, ...);
int puts(const char*);


/* Global Variable Definitions and Initialization */
const int n = 10;


/* Function Bodies */





int inc_2(int c2ssa_a) {
  c2ssa_inc_2_call = rand();
  return ((c2ssa_inc_2_call % c2ssa_a) + c2ssa_a);
}

int inc_1(int c2ssa_a) {
  c2ssa_inc_1_call = rand();
  return ((c2ssa_inc_1_call % c2ssa_a) + c2ssa_a);
}

int main(void) {
  puts("mod: 1");
  puts("mod: 1");
  c2ssa_main_call12_2e_2 = inc_1(2);
  printf("%d\n", c2ssa_main_call12_2e_2);
  puts("mod: 0");
  c2ssa_main_call12_2e_4 = inc_2(4);
  printf("%d\n", c2ssa_main_call12_2e_4);
  puts("mod: 1");
  puts("mod: 3");
  printf("Point: %d, %d\n", 23, 26);
  return 0;
}
