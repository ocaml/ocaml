/* 1. Allocation functions doing the same work as the macros in the
      case where [Setup_for_gc] and [Restore_after_gc] are no-ops.
   2. Convenience functions related to allocation.
*/

#include <string.h>
#include "alloc.h"
#include "major_gc.h"
#include "memory.h"
#include "mlvalues.h"
#include "stacks.h"

#define Setup_for_gc
#define Restore_after_gc

value alloc (wosize, tag)
     mlsize_t wosize;
     tag_t tag;
{
  value result;
  
  Assert (wosize > 0 && wosize <= Max_young_wosize);
  Alloc_small (result, wosize, tag);
  return result;
}

value alloc_tuple(n)
     mlsize_t n;
{
  return alloc(n, 0);
}

value alloc_string (len)
     mlsize_t len;
{
  value result;
  mlsize_t offset_index;
  mlsize_t wosize = (len + sizeof (value)) / sizeof (value);

  if (wosize <= Max_young_wosize) {
    Alloc_small (result, wosize, String_tag);
  }else{
    result = alloc_shr (wosize, String_tag);
  }
  Field (result, wosize - 1) = 0;
  offset_index = Bsize_wsize (wosize) - 1;
  Byte (result, offset_index) = offset_index - len;
  return result;
}

value alloc_final (len, fun, mem, max)
     mlsize_t len;
     final_fun fun;
     mlsize_t mem, max;
{
  value result = alloc_shr (len, Final_tag);

  Field (result, 0) = (value) fun;
  adjust_gc_speed (mem, max);
  return result;
}

value copy_double(d)
     double d;
{
  value res;

  Alloc_small(res, Double_wosize, Double_tag);
  Store_double_val(res, d);
  return res;
}

value copy_string(s)
     char * s;
{
  int len;
  value res;

  len = strlen(s);
  res = alloc_string(len);
  bcopy(s, String_val(res), len);
  return res;
}

value alloc_array(funct, arr)
     value (*funct) P((char *));
     char ** arr;
{
  mlsize_t nbr, n;
  value v;

  nbr = 0;
  while (arr[nbr] != 0) nbr++;
  if (nbr == 0) {
    v = Atom(0);
  } else {
    while (extern_sp - nbr <= stack_low)
      realloc_stack();
    for (n = 0; n < nbr; n++)
      *--extern_sp = funct(arr[n]);
    if (nbr < Max_young_wosize) {
      v = alloc(nbr, 0);
      n = nbr;
      while (n-- > 0) Field (v, n) = *extern_sp++;
    } else {
      v = alloc_shr(nbr, 0);
      n = nbr;
      while (n-- > 0) initialize (&Field(v, n), *extern_sp++);
    }
  }
  return v;
}

value copy_string_array(arr)
     char ** arr;
{
  return alloc_array(copy_string, arr);
}

int convert_flag_list(list, flags)
     value list;
     int * flags;
{
  int res;
  res = 0;
  while (list != Val_int(0)) {
    res |= flags[Int_val(Field(list, 0))];
    list = Field(list, 1);
  }
  return res;
}
