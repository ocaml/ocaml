/* For testing global root registration */

#include "mlvalues.h"
#include "memory.h"
#include "alloc.h"

struct block { value v; };

#define Block_val(v) ((struct block *) (v))

value gb_register(value v)
{
  struct block * b = stat_alloc(sizeof(struct block));
  b->v = v;
  register_global_root(&(b->v));
  return (value) b;
}

value gb_get(value vblock)
{
  return Block_val(vblock)->v;
}

value gb_remove(value vblock)
{
  remove_global_root(&(Block_val(vblock)->v));
  return Val_unit;
}

