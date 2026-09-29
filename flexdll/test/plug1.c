#include <stdio.h>

#include "api.h"

int x = 10;
void dump_x(void) { printf("AAA\nx=%i\n", x); }
void torun(void) { api1("plug1.torun();"); api2("plug1.torun();"); }
