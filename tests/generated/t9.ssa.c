
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
int c2ssa_main_i_2e_1;
int c2ssa_main_j_2e_1;
int c2ssa_main_i_2e_1_2e_1;
int c2ssa_main_j_2e_1_2e_1;
int c2ssa_main_i_2e_1_2e_2;
int c2ssa_main_j_2e_1_2e_2;
int c2ssa_main_i_2e_1_2e_3;
int c2ssa_main_j_2e_1_2e_3;
int c2ssa_main_i_2e_1_2e_4;
int c2ssa_main_j_2e_1_2e_4;
int c2ssa_main_i_2e_1_2e_5;
int c2ssa_main_j_2e_1_2e_5;
int c2ssa_main_i_2e_1_2e_6;
int c2ssa_main_j_2e_1_2e_6;
int c2ssa_main_i_2e_1_2e_7;
int c2ssa_main_j_2e_1_2e_7;
int c2ssa_main_i_2e_1_2e_8;
int c2ssa_main_j_2e_1_2e_8;

/* Types Declarations */

/* Function definitions */

/* Types Definitions */

/* External Global Variable Declarations */

/* Function Declarations */
int input(void);
int rand(void);
int main(void);
int printf(const char*, ...);


/* Global Variable Definitions and Initialization */


/* Function Bodies */





int main(void) {
  c2ssa_main_call = rand();
  c2ssa_main_call1 = rand();
  c2ssa_main_i_2e_1 = (((((c2ssa_main_call1 + c2ssa_main_call) & 1) == 0) ? 2 : 1)) + c2ssa_main_call;
  c2ssa_main_j_2e_1 = (((((c2ssa_main_call1 + c2ssa_main_call) & 1) == 0) ? 3 : 7)) + c2ssa_main_call1;
  c2ssa_main_i_2e_1_2e_1 = (((((c2ssa_main_j_2e_1 + c2ssa_main_i_2e_1) & 1) == 0) ? 2 : 1)) + c2ssa_main_i_2e_1;
  c2ssa_main_j_2e_1_2e_1 = (((((c2ssa_main_j_2e_1 + c2ssa_main_i_2e_1) & 1) == 0) ? 3 : 7)) + c2ssa_main_j_2e_1;
  c2ssa_main_i_2e_1_2e_2 = (((((c2ssa_main_j_2e_1_2e_1 + c2ssa_main_i_2e_1_2e_1) & 1) == 0) ? 2 : 1)) + c2ssa_main_i_2e_1_2e_1;
  c2ssa_main_j_2e_1_2e_2 = (((((c2ssa_main_j_2e_1_2e_1 + c2ssa_main_i_2e_1_2e_1) & 1) == 0) ? 3 : 7)) + c2ssa_main_j_2e_1_2e_1;
  c2ssa_main_i_2e_1_2e_3 = (((((c2ssa_main_j_2e_1_2e_2 + c2ssa_main_i_2e_1_2e_2) & 1) == 0) ? 2 : 1)) + c2ssa_main_i_2e_1_2e_2;
  c2ssa_main_j_2e_1_2e_3 = (((((c2ssa_main_j_2e_1_2e_2 + c2ssa_main_i_2e_1_2e_2) & 1) == 0) ? 3 : 7)) + c2ssa_main_j_2e_1_2e_2;
  c2ssa_main_i_2e_1_2e_4 = (((((c2ssa_main_j_2e_1_2e_3 + c2ssa_main_i_2e_1_2e_3) & 1) == 0) ? 2 : 1)) + c2ssa_main_i_2e_1_2e_3;
  c2ssa_main_j_2e_1_2e_4 = (((((c2ssa_main_j_2e_1_2e_3 + c2ssa_main_i_2e_1_2e_3) & 1) == 0) ? 3 : 7)) + c2ssa_main_j_2e_1_2e_3;
  c2ssa_main_i_2e_1_2e_5 = (((((c2ssa_main_j_2e_1_2e_4 + c2ssa_main_i_2e_1_2e_4) & 1) == 0) ? 2 : 1)) + c2ssa_main_i_2e_1_2e_4;
  c2ssa_main_j_2e_1_2e_5 = (((((c2ssa_main_j_2e_1_2e_4 + c2ssa_main_i_2e_1_2e_4) & 1) == 0) ? 3 : 7)) + c2ssa_main_j_2e_1_2e_4;
  c2ssa_main_i_2e_1_2e_6 = (((((c2ssa_main_j_2e_1_2e_5 + c2ssa_main_i_2e_1_2e_5) & 1) == 0) ? 2 : 1)) + c2ssa_main_i_2e_1_2e_5;
  c2ssa_main_j_2e_1_2e_6 = (((((c2ssa_main_j_2e_1_2e_5 + c2ssa_main_i_2e_1_2e_5) & 1) == 0) ? 3 : 7)) + c2ssa_main_j_2e_1_2e_5;
  c2ssa_main_i_2e_1_2e_7 = (((((c2ssa_main_j_2e_1_2e_6 + c2ssa_main_i_2e_1_2e_6) & 1) == 0) ? 2 : 1)) + c2ssa_main_i_2e_1_2e_6;
  c2ssa_main_j_2e_1_2e_7 = (((((c2ssa_main_j_2e_1_2e_6 + c2ssa_main_i_2e_1_2e_6) & 1) == 0) ? 3 : 7)) + c2ssa_main_j_2e_1_2e_6;
  c2ssa_main_i_2e_1_2e_8 = (((((c2ssa_main_j_2e_1_2e_7 + c2ssa_main_i_2e_1_2e_7) & 1) == 0) ? 2 : 1)) + c2ssa_main_i_2e_1_2e_7;
  c2ssa_main_j_2e_1_2e_8 = (((((c2ssa_main_j_2e_1_2e_7 + c2ssa_main_i_2e_1_2e_7) & 1) == 0) ? 3 : 7)) + c2ssa_main_j_2e_1_2e_7;
  printf("ij: %d, %d\n", ((((((c2ssa_main_j_2e_1_2e_8 + c2ssa_main_i_2e_1_2e_8) & 1) == 0) ? 2 : 1)) + c2ssa_main_i_2e_1_2e_8), ((((((c2ssa_main_j_2e_1_2e_8 + c2ssa_main_i_2e_1_2e_8) & 1) == 0) ? 3 : 7)) + c2ssa_main_j_2e_1_2e_8));
  return 0;
}
