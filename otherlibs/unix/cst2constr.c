#include <mlvalues.h>
#include <fail.h>
#include "cst2constr.h"

value cst_to_constr(n, tbl, size, deflt)
     int n;
     int * tbl;
     int size;
     int deflt;
{
  int i;
  for (i = 0; i < size; i++)
    if (n == tbl[i]) return Val_int(i);
  return Val_int(deflt);
}
