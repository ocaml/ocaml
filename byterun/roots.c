/* To walk the memory roots for garbage collection */

#include "memory.h"
#include "misc.h"
#include "mlvalues.h"
#include "roots.h"
#include "stacks.h"

value * local_roots = NULL;

struct global_root {
  value * root;
  struct global_root * next;
};
  
static struct global_root * global_roots = NULL;

void scan_local_roots (copy_fn)
     void (*copy_fn) ();
{
  register value * sp;
  value * block;
  struct global_root * gr;

  /* The stack */
  for (sp = extern_sp; sp < stack_high; sp++) {
    copy_fn (sp, *sp);
  }
  /* Local C roots */
  for (block = local_roots; block != NULL; block = (value *) block [1]){
    for (sp = block - (long) block [0]; sp < block; sp++){
      copy_fn (sp, *sp);
    }
  }
  /* Global C roots */
  for (gr = global_roots; gr != NULL; gr = gr->next) {
    copy_fn(gr->root, *(gr->root));
  }
}

void register_global_root(r)
     value * r;
{
  struct global_root * gr;
  gr = (struct global_root *) stat_alloc(sizeof(struct global_root));
  gr->root = r;
  gr->next = global_roots;
  global_roots = gr;
}
