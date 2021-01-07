
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
void f8(void);
int printf(const char*, ...);
void f6(void);
void f5(void);
void f7(void);
void f4(void);
void f3(void);
void f2(void);
void f1(void);
int main(void);
int puts(const char*);


/* Global Variable Definitions and Initialization */


/* Function Bodies */



















void f8_7(void) {
  puts("f8");
}

void f6_2(void) {
  puts("f6");
  f8_7();
}

void f8_6(void) {
  puts("f8");
}

void f5_4(void) {
  puts("f5");
  f8_6();
}

void f3_2(void) {
  puts("f3");
  f5_4();
  f6_2();
}

void f8_5(void) {
  puts("f8");
}

void f6_1(void) {
  puts("f6");
  f8_5();
}

void f8_4(void) {
  puts("f8");
}

void f5_3(void) {
  puts("f5");
  f8_4();
}

void f3_1(void) {
  puts("f3");
  f5_3();
  f6_1();
}

void f8_3(void) {
  puts("f8");
}

void f8_2(void) {
  puts("f8");
}

void f5_2(void) {
  puts("f5");
  f8_2();
}

void f7_1(void) {
  puts("f7");
  f5_2();
  f8_3();
}

void f8_1(void) {
  puts("f8");
}

void f5_1(void) {
  puts("f5");
  f8_1();
}

void f4_1(void) {
  puts("f4");
  f5_1();
  f7_1();
}

void f2_1(void) {
  puts("f2");
  f4_1();
  f3_1();
}

void f1_1(void) {
  puts("f1");
  f2_1();
  f3_2();
}

int main(void) {
  f1_1();
  return 0;
}
