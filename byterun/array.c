/* Operations on arrays */

#include "alloc.h"
#include "fail.h"
#include "memory.h"
#include "misc.h"
#include "mlvalues.h"

value make_vect(len, init)      /* ML */
     value len, init;
{
  value res;
  mlsize_t size, i;
  Push_roots(root, 1);

  size = Long_val(len);
  if (size > Max_wosize) {
    Pop_roots();
    invalid_argument("Array.new");
  }
  if (size == 0) {
    res = Atom(0);
  }
  else if (size < Max_young_wosize) {
    root[0] = init;
    res = alloc(size, 0);
    init = root[0];
    for (i = 0; i < size; i++) Field(res, i) = init;
  }
  else if (Is_block(init) && Is_young(init)) {
    root[0] = init;
    minor_collection();
    res = alloc_shr(size, 0);
    init = root[0];
    for (i = 0; i < size; i++) Field(res, i) = init;
  }
  else {
    root[0] = init;
    res = alloc_shr(size, 0);
    init = root[0];
    for (i = 0; i < size; i++) initialize(&Field(res, i), init);
  }
  Pop_roots();
  return res;
}
