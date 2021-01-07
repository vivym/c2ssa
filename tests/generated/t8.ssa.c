
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
int c2ssa_input_2_call;
int c2ssa_input_1_call;
int c2ssa_main_call;
int c2ssa_main_call1;
int c2ssa_main_add;
int c2ssa_main_add2;
int c2ssa_main_j_2e_0;
int c2ssa_main_i_2e_0;
int c2ssa_main_add11;
int c2ssa_main_j_2e_1;
int c2ssa_main_i_2e_1;
int c2ssa_main_add19;
int c2ssa_main_j_2e_2;
int c2ssa_main_i_2e_2;
int c2ssa_main_add27;
int c2ssa_main_j_2e_3;
int c2ssa_main_i_2e_3;
int c2ssa_main_add35;
int c2ssa_main_j_2e_4;
int c2ssa_main_i_2e_4;
int c2ssa_main_add43;
int c2ssa_main_j_2e_5;
int c2ssa_main_i_2e_5;
int c2ssa_main_add51;
int c2ssa_main_j_2e_6;
int c2ssa_main_i_2e_6;
int c2ssa_main_add59;
int c2ssa_main_j_2e_7;
int c2ssa_main_i_2e_7;
int c2ssa_main_add67;
int c2ssa_main_j_2e_8;
int c2ssa_main_i_2e_8;
int c2ssa_main_add75;
int c2ssa_main_j_2e_9;
int c2ssa_main_i_2e_9;
int c2ssa_main_add83;
int c2ssa_main_j_2e_10;
int c2ssa_main_i_2e_10;
int c2ssa_main_add91;
int c2ssa_main_j_2e_11;
int c2ssa_main_i_2e_11;
int c2ssa_main_add99;
int c2ssa_main_j_2e_12;
int c2ssa_main_i_2e_12;
int c2ssa_main_add107;
int c2ssa_main_j_2e_13;
int c2ssa_main_i_2e_13;
int c2ssa_main_add115;
int c2ssa_main_j_2e_14;
int c2ssa_main_i_2e_14;
int c2ssa_main_add118;
int c2ssa_main_div;
int c2ssa_main_j_2e_14_2e_sink;
int c2ssa_main_j_2e_14_2e_sink__PHI_TEMPORARY;
int c2ssa_main_d_2e_0;
int c2ssa_main_d_2e_0__PHI_TEMPORARY;

/* Types Declarations */

/* Function definitions */

/* Types Definitions */

/* External Global Variable Declarations */

/* Function Declarations */
void print(int);
int printf(const char*, ...);
int input(void);
int rand(void);
int main(void);
void srand(int);


/* Global Variable Definitions and Initialization */


/* Function Bodies */







void print_6(int c2ssa_x) {
  printf("%d\n", c2ssa_x);
}

void print_5(int c2ssa_x) {
  printf("%d\n", c2ssa_x);
}

void print_4(int c2ssa_x) {
  printf("%d\n", c2ssa_x);
}

void print_3(int c2ssa_x) {
  printf("%d\n", c2ssa_x);
}

void print_2(int c2ssa_x) {
  printf("%d\n", c2ssa_x);
}

void print_1(int c2ssa_x) {
  printf("%d\n", c2ssa_x);
}

int input_2(void) {
  c2ssa_input_2_call = rand();
  return c2ssa_input_2_call;
}

int input_1(void) {
  c2ssa_input_1_call = rand();
  return c2ssa_input_1_call;
}

int main(void) {
  srand(10);
  c2ssa_main_call = input_1();
  c2ssa_main_call1 = input_2();
  c2ssa_main_add = c2ssa_main_call1 + c2ssa_main_call;
  c2ssa_main_add2 = c2ssa_main_add + 10;
  c2ssa_main_j_2e_0 = ((c2ssa_main_call < 10) ? (c2ssa_main_call1 + 3) : c2ssa_main_add2);
  c2ssa_main_i_2e_0 = ((c2ssa_main_call < 10) ? (c2ssa_main_call + 2) : c2ssa_main_add);
  c2ssa_main_add11 = c2ssa_main_j_2e_0 + c2ssa_main_i_2e_0;
  c2ssa_main_j_2e_1 = ((c2ssa_main_j_2e_0 > 100) ? (c2ssa_main_j_2e_0 + -3) : (c2ssa_main_add11 + 10));
  c2ssa_main_i_2e_1 = ((c2ssa_main_j_2e_0 > 100) ? (c2ssa_main_i_2e_0 + 2) : c2ssa_main_add11);
  c2ssa_main_add19 = c2ssa_main_j_2e_1 + c2ssa_main_i_2e_1;
  c2ssa_main_j_2e_2 = ((c2ssa_main_j_2e_1 > 100) ? (c2ssa_main_j_2e_1 + -3) : (c2ssa_main_add19 + 10));
  c2ssa_main_i_2e_2 = ((c2ssa_main_j_2e_1 > 100) ? (c2ssa_main_i_2e_1 + 2) : c2ssa_main_add19);
  c2ssa_main_add27 = c2ssa_main_j_2e_2 + c2ssa_main_i_2e_2;
  c2ssa_main_j_2e_3 = ((c2ssa_main_j_2e_2 > 100) ? (c2ssa_main_j_2e_2 + -3) : (c2ssa_main_add27 + 10));
  c2ssa_main_i_2e_3 = ((c2ssa_main_j_2e_2 > 100) ? (c2ssa_main_i_2e_2 + 2) : c2ssa_main_add27);
  c2ssa_main_add35 = c2ssa_main_j_2e_3 + c2ssa_main_i_2e_3;
  c2ssa_main_j_2e_4 = ((c2ssa_main_j_2e_3 > 100) ? (c2ssa_main_j_2e_3 + -3) : (c2ssa_main_add35 + 10));
  c2ssa_main_i_2e_4 = ((c2ssa_main_j_2e_3 > 100) ? (c2ssa_main_i_2e_3 + 2) : c2ssa_main_add35);
  c2ssa_main_add43 = c2ssa_main_j_2e_4 + c2ssa_main_i_2e_4;
  c2ssa_main_j_2e_5 = ((c2ssa_main_j_2e_4 > 100) ? (c2ssa_main_j_2e_4 + -3) : (c2ssa_main_add43 + 10));
  c2ssa_main_i_2e_5 = ((c2ssa_main_j_2e_4 > 100) ? (c2ssa_main_i_2e_4 + 2) : c2ssa_main_add43);
  c2ssa_main_add51 = c2ssa_main_j_2e_5 + c2ssa_main_i_2e_5;
  c2ssa_main_j_2e_6 = ((c2ssa_main_j_2e_5 > 100) ? (c2ssa_main_j_2e_5 + -3) : (c2ssa_main_add51 + 10));
  c2ssa_main_i_2e_6 = ((c2ssa_main_j_2e_5 > 100) ? (c2ssa_main_i_2e_5 + 2) : c2ssa_main_add51);
  c2ssa_main_add59 = c2ssa_main_j_2e_6 + c2ssa_main_i_2e_6;
  c2ssa_main_j_2e_7 = ((c2ssa_main_j_2e_6 > 100) ? (c2ssa_main_j_2e_6 + -3) : (c2ssa_main_add59 + 10));
  c2ssa_main_i_2e_7 = ((c2ssa_main_j_2e_6 > 100) ? (c2ssa_main_i_2e_6 + 2) : c2ssa_main_add59);
  c2ssa_main_add67 = c2ssa_main_j_2e_7 + c2ssa_main_i_2e_7;
  c2ssa_main_j_2e_8 = ((c2ssa_main_j_2e_7 > 100) ? (c2ssa_main_j_2e_7 + -3) : (c2ssa_main_add67 + 10));
  c2ssa_main_i_2e_8 = ((c2ssa_main_j_2e_7 > 100) ? (c2ssa_main_i_2e_7 + 2) : c2ssa_main_add67);
  c2ssa_main_add75 = c2ssa_main_j_2e_8 + c2ssa_main_i_2e_8;
  c2ssa_main_j_2e_9 = ((c2ssa_main_j_2e_8 > 100) ? (c2ssa_main_j_2e_8 + -3) : (c2ssa_main_add75 + 10));
  c2ssa_main_i_2e_9 = ((c2ssa_main_j_2e_8 > 100) ? (c2ssa_main_i_2e_8 + 2) : c2ssa_main_add75);
  c2ssa_main_add83 = c2ssa_main_j_2e_9 + c2ssa_main_i_2e_9;
  c2ssa_main_j_2e_10 = ((c2ssa_main_j_2e_9 > 100) ? (c2ssa_main_j_2e_9 + -3) : (c2ssa_main_add83 + 10));
  c2ssa_main_i_2e_10 = ((c2ssa_main_j_2e_9 > 100) ? (c2ssa_main_i_2e_9 + 2) : c2ssa_main_add83);
  c2ssa_main_add91 = c2ssa_main_j_2e_10 + c2ssa_main_i_2e_10;
  c2ssa_main_j_2e_11 = ((c2ssa_main_j_2e_10 > 100) ? (c2ssa_main_j_2e_10 + -3) : (c2ssa_main_add91 + 10));
  c2ssa_main_i_2e_11 = ((c2ssa_main_j_2e_10 > 100) ? (c2ssa_main_i_2e_10 + 2) : c2ssa_main_add91);
  c2ssa_main_add99 = c2ssa_main_j_2e_11 + c2ssa_main_i_2e_11;
  c2ssa_main_j_2e_12 = ((c2ssa_main_j_2e_11 > 100) ? (c2ssa_main_j_2e_11 + -3) : (c2ssa_main_add99 + 10));
  c2ssa_main_i_2e_12 = ((c2ssa_main_j_2e_11 > 100) ? (c2ssa_main_i_2e_11 + 2) : c2ssa_main_add99);
  c2ssa_main_add107 = c2ssa_main_j_2e_12 + c2ssa_main_i_2e_12;
  c2ssa_main_j_2e_13 = ((c2ssa_main_j_2e_12 > 100) ? (c2ssa_main_j_2e_12 + -3) : (c2ssa_main_add107 + 10));
  c2ssa_main_i_2e_13 = ((c2ssa_main_j_2e_12 > 100) ? (c2ssa_main_i_2e_12 + 2) : c2ssa_main_add107);
  c2ssa_main_add115 = c2ssa_main_j_2e_13 + c2ssa_main_i_2e_13;
  c2ssa_main_j_2e_14 = ((c2ssa_main_j_2e_13 > 100) ? (c2ssa_main_j_2e_13 + -3) : (c2ssa_main_add115 + 10));
  c2ssa_main_i_2e_14 = ((c2ssa_main_j_2e_13 > 100) ? (c2ssa_main_i_2e_13 + 2) : c2ssa_main_add115);
  c2ssa_main_add118 = c2ssa_main_j_2e_14 + c2ssa_main_i_2e_14;
  if ((c2ssa_main_add118 < 100)) {
    c2ssa_main_j_2e_14_2e_sink__PHI_TEMPORARY = c2ssa_main_add118;   /* for PHI node */
    c2ssa_main_d_2e_0__PHI_TEMPORARY = c2ssa_main_add118;   /* for PHI node */
    goto c2ssa_if_2e_end123;
  } else {
    goto c2ssa_if_2e_else122;
  }

c2ssa_if_2e_else122:
  c2ssa_main_div = c2ssa_main_i_2e_14 / c2ssa_main_j_2e_14;
  print_1(c2ssa_main_div);
  print_2(c2ssa_main_i_2e_14);
  c2ssa_main_j_2e_14_2e_sink__PHI_TEMPORARY = c2ssa_main_j_2e_14;   /* for PHI node */
  c2ssa_main_d_2e_0__PHI_TEMPORARY = c2ssa_main_div;   /* for PHI node */
  goto c2ssa_if_2e_end123;

c2ssa_if_2e_end123:
  c2ssa_main_j_2e_14_2e_sink = c2ssa_main_j_2e_14_2e_sink__PHI_TEMPORARY;
  c2ssa_main_d_2e_0 = c2ssa_main_d_2e_0__PHI_TEMPORARY;
  print_3(c2ssa_main_j_2e_14_2e_sink);
  print_4(c2ssa_main_add2);
  print_5(c2ssa_main_d_2e_0);
  print_6((c2ssa_main_d_2e_0 + c2ssa_main_add2));
  return 0;
}
