/* A very simplified runtime system for the native code compiler */

#include <stdio.h>
#include <stdlib.h>

int heapsize = 1024 * 1024;     /* 1M */
char * young_start, * young_ptr, * young_end;
char * remembered_set[4096];
char ** remembered_ptr = remembered_set;
char ** remembered_end = remembered_set + 4096;

void garbage_collection(request)
     int request;
{
  young_start = malloc(heapsize);
  if (young_start == NULL) {
    fprintf(stderr, "Out of heap size\n");
    exit(2);
  }
  young_end = young_start + heapsize;
  young_ptr = young_end - request;
}

void realloc_remembered()
{
  remembered_ptr = remembered_set;
}

extern int caml_start_program();

typedef long value;

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

value equal(v1, v2)
     value v1, v2;
{
  value * p1, * p2;
  value hdr1, hdr2, size, i;

 tailcall:
  if (v1 == v2) return 3;       /* true */
  if (v1 & 1) return 1;         /* false */
  if (v1 & 1) return 1;         /* false */
  p1 = (value *) v1;
  p2 = (value *) v2;
  hdr1 = p1[-1];
  hdr2 = p2[-1];
  if (hdr1 != hdr2) return 1;   /* false */
  size = hdr1 >> 10;
  switch(hdr1 & 0xFF) {
  case 251:
    fprintf(stderr, "equal between functions\n");
    exit(2);
  case 253:
    for (i = 0; i < size; i++)
      if (p1[i] != p2[i]) return 1;
    return 3;
  case 254:
    if (*((double *) v1) = *((double *) v2)) return 3; else return 1;
  default:
    for (i = 0; i < size-1; i++)
      if (equal(p1[i], p2[i]) == 1) return 1;
    v1 = p1[i];
    v2 = p2[i];
    goto tailcall;
  }
}

value notequal(v1, v2)
     value v1, v2;
{
  return (4 - equal(v1, v2));
}

#define COMPARISON(name) \
value name(v1, v2) \
     value v1, v2; \
{ \
  fprintf(stderr, "%s not implemented.\n", #name); \
  exit(2); \
}

COMPARISON(greaterequal)
COMPARISON(lessequal)
COMPARISON(greaterthan)
COMPARISON(lessthan)

value alloc_dummy(size)
     int size;
{
  value * block;
  int bsize, i;

  bsize = (size + 1) * sizeof(value);
  young_ptr -= bsize;
  if (young_ptr < young_start) garbage_collection(bsize);
  block = (value *) young_ptr + 1;
  block[-1] = size << 10;
  for (i = 0; i < size; i++) block[i] = 0;
  return (value) block;
}

static struct {
  value header;
  char data[16];
} match_failure_id = { 0, "Match_failure" }; /* to be revised */

char * Match_failure = match_failure_id.data;

int main(argc, argv)
     int argc;
     char ** argv;
{
  garbage_collection(0);
  if (caml_start_program() != 0) {
    fprintf(stderr, "Uncaught exception\n");
    exit(2);
  }
  return 0;
}

