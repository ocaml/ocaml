/* A very simplified runtime system for the native code compiler */

#include <stdio.h>
#include <stdlib.h>
#include "mlvalues.h"

extern int caml_start_program();

value print_int(n)
     value n;
{
  printf("%d", n>>1);
  return 1;
}

value print_string(s)
     value s;
{
  printf("%s", (char *) s);
  return 1;
}

value print_char(c)
     value c;
{
  printf("%c", c>>1);
  return 1;
}

static struct {
  value header;
  char data[16];
} match_failure_id = {
  ((16 / sizeof(value)) << 11) + 0xFC,
  "Match_failure\0\0\2"
};

char * Match_failure = match_failure_id.data;

int main(argc, argv)
     int argc;
     char ** argv;
{
  init_heap();
  if (caml_start_program() != 0) {
    fprintf(stderr, "Uncaught exception\n");
    exit(2);
  }
  return 0;
}

