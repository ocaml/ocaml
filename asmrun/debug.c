#include <stdio.h>
#include "misc.h"
#include "mlvalues.h"

char * young_start, * young_ptr, * young_end;
char * old_start, * old_ptr, * old_end;
value ** remembered_start, ** remembered_ptr, ** remembered_end;

void failed_assert(file, line)
     char * file;
     int line;
{
  fprintf(stderr, "Failed assertion, file %s, line %d\n", file, line);
  exit(2);
}

extern unsigned long _etext;
long current_break;

/* Check that an object is (reasonably) well-formed */

#define MAX_SIZE 63
#define MAX_TAG 1

void check_field(v)
     value v;
{
  if (Is_int(v)) return;
  Assert((v & (sizeof(value) - 1)) == 0);
  Assert(v >= (long) &_etext && v <= (long) current_break);
  if ((char *)v > young_start && (char *)v <= young_end) {
    Assert((char *)v > young_ptr);
  }
}

void check_value(v)
     value v;
{
  header_t hdr, sz;
  int i;

  if (Is_int(v)) return;
  check_field(v);
  hdr = Header_val(v);
  sz = Size_val(v);
  Assert((hdr & 0x300) == 0);
  switch(Tag_header(hdr)) {
  case Double_tag:
    Assert(sz == sizeof(double) / sizeof(value));
    break;
  case String_tag:
    i = ((char *)v)[sz * sizeof(value) - 1];
    Assert(i >= 0 && i < sizeof(value));
    Assert(((char *)v)[sz * sizeof(value) - 1 - i] == 0);
    break;
  case Abstract_tag:
  case Finalized_tag:
    Assert(0);
    break;
  case Infix_tag:
    v -= sz * sizeof(value);
    Assert(Header_val(v) == Closure_tag);
    check_value(v);
    break;
  case Closure_tag:
    Assert(Field(v, 0) < (long)&_etext);
    if (Field(v, 1) == Val_int(1)) {
      i = 2;
    } else {
      Assert(Is_int(Field(v, 1)));
      Assert(Field(v, 2) < (long)&_etext);
      i = 3;
    }
    while(1) {
      hdr = (header_t) Field(v, i);
      if (Tag_header(hdr) != Infix_tag) break;
      i++;
      Assert(Size_header(hdr) == i);
      Assert(Field(v, i) < (long)&_etext);
      i++;
      if (Field(v, i) == Val_int(1)) {
        i++;
      } else {
        Assert(Is_int(Field(v, i)));
        i++;
        Assert(Field(v, i) < (long)&_etext);
        i++;
      }
    }
    for (/*nothing*/; i < sz; i++) check_field(Field(v, i));
    break;
  default:
#ifdef MAX_SIZE
    Assert(sz <= MAX_SIZE);
#endif
#ifdef MAX_TAG
    Assert(Tag_header(hdr) <= MAX_TAG);
#endif
    for (i = 0; i < sz; i++) check_field(Field(v, i));
    break;
  }
}

/* Check that a heap chunk is well-formed */

void check_heap(start, end)
     char * start;
     char * end;
{
  char * p;
  value v;

  current_break = sbrk(0);
  p = start;
  while (p < end) {
    v = (value)(p + sizeof(header_t));
    check_value(v);
    p += sizeof(header_t) + Size_val(v) * sizeof(value);
  }
  Assert(p == end);
}

/* Check the globals */

extern value * caml_globals[];

void check_globals()
{
  int i;
  current_break = sbrk(0);
  for (i = 0; caml_globals[i] != 0; i++) {
    value v = *(caml_globals[i]);
    if (v != 0) check_value(v);
  }
}
