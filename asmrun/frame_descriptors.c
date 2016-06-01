#include "frame_descriptors.h"
#include "stack.h"
#include "caml/platform.h"

typedef struct link {
  intnat* frametable;
  struct link *next;
} link;

#define iter_list(list,lnk) \
  for (lnk = list; lnk != NULL; lnk = lnk->next)


#define Hash_retaddr(addr, mask)                          \
  (((uintnat)(addr) >> 3) & (mask))

static value build_frame_descriptors(link* frametables)
{
  intnat num_descr, tblsize, i, j, len;
  intnat * tbl;
  frame_descr * d;
  uintnat nextd;
  uintnat h;
  link *lnk;
  value descriptor_table;
  frame_descr** frame_descriptors;

  /* Count the frame descriptors */
  num_descr = 0;
  iter_list(frametables,lnk) {
    num_descr += *lnk->frametable;
  }

  /* The size of the hashtable is a power of 2 greater or equal to
     2 times the number of descriptors */
  tblsize = 4;
  while (tblsize < 2 * num_descr) tblsize *= 2;

  /* Allocate the hash table */
  Assert(sizeof(frame_descr*) == sizeof(value));
  descriptor_table = caml_alloc_shr(tblsize, Abstract_tag);
  frame_descriptors = Data_abstract_val(descriptor_table);
  for (i = 0; i < tblsize; i++) frame_descriptors[i] = NULL;

  /* Fill the hash table */
  iter_list(frametables,lnk) {
    tbl = lnk->frametable;
    len = *tbl;
    d = (frame_descr *)(tbl + 1);
    for (j = 0; j < len; j++) {
      h = Hash_retaddr(d->retaddr, tblsize - 1);
      while (frame_descriptors[h] != NULL) {
        h = (h+1) & (tblsize - 1);
      }
      frame_descriptors[h] = d;
      nextd =
        ((uintnat)d +
         sizeof(char *) + sizeof(short) + sizeof(short) +
         sizeof(short) * d->num_live + sizeof(frame_descr *) - 1)
        & -sizeof(frame_descr *);
      if (d->frame_size & 1) nextd += 8;
      d = (frame_descr *) nextd;
    }
  }
  return descriptor_table;
}


static caml_plat_mutex descr_mutex;
static link* frametables;
static value frame_descriptor_table = Val_unit;

static link *cons(intnat *frametable, link *tl) {
  link *lnk = caml_stat_alloc(sizeof(link));
  lnk->frametable = frametable;
  lnk->next = tl;
  return lnk;
}

void caml_init_frame_descriptors(void)
{
  int i;

  Assert(frame_descriptor_table == Val_unit);
  caml_plat_mutex_init(&descr_mutex);

  caml_plat_lock(&descr_mutex);
  for (i = 0; caml_frametable[i] != 0; i++)
    frametables = cons(caml_frametable[i], frametables);

  frame_descriptor_table = build_frame_descriptors(frametables);
  caml_plat_unlock(&descr_mutex);
}

void caml_register_frametable(intnat *table)
{
  Assert(frame_descriptor_table != Val_unit);

  caml_plat_lock(&descr_mutex);

  frametables = cons(table, frametables);
  frame_descriptor_table = build_frame_descriptors(frametables);
  /* old frame_descriptor_table is GC'd eventually */

  caml_plat_unlock(&descr_mutex);
}




void caml_scan_stack_roots(scanning_action f,
                           char* sp, uintnat retaddr, value* regs) {
  frame_descr * d;
  uintnat h;
  int n, ofs;
#ifdef Stack_grows_upwards
  short * p;  /* PR#4339: stack offsets are negative in this case */
#else
  unsigned short * p;
#endif
  frame_descr** frame_descriptors;
  int frame_descriptors_mask;
  value* root;


  /* The global roots.
     FIXME: These should be promoted, and not scanned here.
     FIXME: caml_globals_inited makes assumptions about store ordering.
  */
  value glob;
  int i, j;
  for (i = 0; i <= caml_globals_inited && caml_globals[i] != 0; i++) {
    glob = caml_globals[i];
    for (j = 0; j < Wosize_val(glob); j++){
      f(Op_val(glob)[j], &Op_val(glob)[j]);
    }
  }


  f(frame_descriptor_table, &frame_descriptor_table);

  if (sp != NULL) {
    Assert(frame_descriptor_table != Val_unit);
    frame_descriptors = Data_abstract_val(frame_descriptor_table);
    frame_descriptors_mask = Wosize_val(frame_descriptor_table) - 1;

    while (1) {
      /* Find the descriptor corresponding to the return address */
      h = Hash_retaddr(retaddr, frame_descriptors_mask);
      while(1) {
        d = frame_descriptors[h];
        if (d->retaddr == retaddr) break;
        h = (h+1) & frame_descriptors_mask;
      }
      if (d->frame_size != 0xFFFF) {
        /* Scan the roots in this frame */
        for (p = d->live_ofs, n = d->num_live; n > 0; n--, p++) {
          ofs = *p;
          if (ofs & 1) {
            root = regs + (ofs >> 1);
          } else {
            root = (value *)(sp + ofs);
          }
          f (*root, root);
        }
        /* Move to next frame */
#ifndef Stack_grows_upwards
        sp += (d->frame_size & 0xFFFC);
#else
        sp -= (d->frame_size & 0xFFFC);
#endif
        retaddr = Saved_return_address(sp);
        /* FIXME: support Already_scanned for POWER */
      } else {
        /* This marks the top of a stack chunk for an ML callback.
           Skip C portion of stack and continue with next ML stack chunk. */
        struct caml_context * next_context = Callback_link(sp);
        sp = next_context->bottom_of_stack;
        retaddr = next_context->last_retaddr;
        regs = next_context->gc_regs;
        /* A null sp means no more ML stack chunks; stop here. */
        if (sp == NULL) break;
      }
    }
  }
}



frame_descr* caml_find_frame_descr(uintnat pc)
{
  frame_descr** frame_descriptors;
  int frame_descriptors_mask;
  frame_descr * d;
  uintnat h;

  Assert(frame_descriptor_table != Val_unit);
  frame_descriptors = Data_abstract_val(frame_descriptor_table);
  frame_descriptors_mask = Wosize_val(frame_descriptor_table) - 1;

  h = Hash_retaddr(pc, frame_descriptors_mask);
  while (1) {
    d = frame_descriptors[h];
    if (d == 0) return NULL; /* can happen if some code compiled without -g */
    if (d->retaddr == pc) break;
    h = (h+1) & frame_descriptors_mask;
  }
  return d;
}
