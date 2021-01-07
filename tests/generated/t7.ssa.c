
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
int c2ssa_main_i;
int c2ssa_main_j;
int c2ssa_main_tmp__3;
int c2ssa_main_tmp__4;

/* Types Declarations */

/* Function definitions */

/* Types Definitions */

/* External Global Variable Declarations */

/* Function Declarations */
int main(void);
int scanf(const char*, ...);
int printf(const char*, ...);


/* Global Variable Definitions and Initialization */


/* Function Bodies */



int main(void) {
  c2ssa_main_i = 0;
  c2ssa_main_j = 0;
  scanf("%d %d", (&c2ssa_main_i), (&c2ssa_main_j));
  c2ssa_main_tmp__3 = c2ssa_main_i;
  c2ssa_main_tmp__4 = c2ssa_main_j;
  c2ssa_main_i = (c2ssa_main_tmp__3 + 10);
  c2ssa_main_j = (c2ssa_main_tmp__4 + 20);
  printf("sum: %d\n", (((c2ssa_main_tmp__4 + 18) * (c2ssa_main_tmp__3 + 9)) + (((c2ssa_main_tmp__4 + c2ssa_main_tmp__3) + 24) + (((c2ssa_main_tmp__4 + 14) * (c2ssa_main_tmp__3 + 7)) + (((c2ssa_main_tmp__4 + c2ssa_main_tmp__3) + 18) + (((c2ssa_main_tmp__4 + 10) * (c2ssa_main_tmp__3 + 5)) + (((c2ssa_main_tmp__4 + c2ssa_main_tmp__3) + 12) + (((c2ssa_main_tmp__4 + 6) * (c2ssa_main_tmp__3 + 3)) + (((c2ssa_main_tmp__4 + c2ssa_main_tmp__3) + 6) + (((c2ssa_main_tmp__4 + 2) * (c2ssa_main_tmp__3 + 1)) + (c2ssa_main_tmp__4 + c2ssa_main_tmp__3)))))))))));
  return 0;
}
