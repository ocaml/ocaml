#include "api.h"

extern int x;

void torun(void) {
  api1("plug2.torun();");
  api2("plug2.torun();");
  /*
  dump_x();
  // printf("XXX\n");
  printf("x = %d\n", x);
  x = 100;
  printf("x = %d\n", x);
  dump_x();
  //  printf("XXX\n");
  */
}
