#include <stdio.h>
#include "mlvalues.h"

value equal(v1, v2)
     value v1, v2;
{
  header_t hdr1, hdr2;
  long size, i;

 tailcall:
  if (v1 == v2) return Val_true;
  if (v1 & 1) return Val_false;
  if (v1 & 1) return Val_false;
  hdr1 = Header_val(v1) & ~Modified_mask;
  hdr2 = Header_val(v2) & ~Modified_mask;
  switch(Tag_header(hdr1)) {
  case Closure_tag:
  case Infix_tag:
    fprintf(stderr, "equal between functions\n");
    exit(2);
  case String_tag:
    if (hdr1 != hdr2) return Val_false;
    size = Size_header(hdr1);
    for (i = 0; i < size; i++)
      if (Field(v1, i) != Field(v2, i)) return Val_false;
    return Val_true;
  case Double_tag:
    if (Double_val(v1) == Double_val(v2))
      return Val_true;
    else
      return Val_false;
  case Abstract_tag:
  case Finalized_tag:
    fprintf(stderr, "equal between abstract types\n");
    exit(2);
  default:
    if (hdr1 != hdr2) return Val_false;
    size = Size_header(hdr1);
    for (i = 0; i < size-1; i++)
      if (equal(Field(v1, i), Field(v2, i)) == Val_false) return Val_false;
    v1 = Field(v1, i);
    v2 = Field(v2, i);
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

