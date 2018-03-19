#define CAML_INTERNALS

#include "frame_descriptors.h"
#include "caml/stack.h"
#include "caml/platform.h"

typedef struct link {
  intnat* frametable;
  struct link *next;
} link;

#define iter_list(list,lnk) \
  for (lnk = list; lnk != NULL; lnk = lnk->next)

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
      if (d->frame_size & 1 &&
          d->frame_size != (unsigned short)-1) {
        nextd += 8;
      }
      d = (frame_descr *) nextd;
    }
  }
  return descriptor_table;
}


static caml_plat_mutex descr_mutex;
static link* frametables;
CAMLexport value caml_frame_descriptor_table = Val_unit;

static link *cons(intnat *frametable, link *tl) {
  link *lnk = caml_stat_alloc(sizeof(link));
  lnk->frametable = frametable;
  lnk->next = tl;
  return lnk;
}

void caml_init_frame_descriptors(void)
{
  int i;

  Assert(caml_frame_descriptor_table == Val_unit);
  caml_plat_mutex_init(&descr_mutex);

  caml_plat_lock(&descr_mutex);
  for (i = 0; caml_frametable[i] != 0; i++)
    frametables = cons(caml_frametable[i], frametables);

  caml_frame_descriptor_table = build_frame_descriptors(frametables);
  caml_plat_unlock(&descr_mutex);
}

void caml_register_frametable(intnat *table)
{
  Assert(caml_frame_descriptor_table != Val_unit);

  caml_plat_lock(&descr_mutex);

  frametables = cons(table, frametables);
  caml_frame_descriptor_table = build_frame_descriptors(frametables);
  /* old frame_descriptor_table is GC'd eventually */

  caml_plat_unlock(&descr_mutex);
}

frame_descr* caml_find_frame_descr(uintnat pc)
{
  frame_descr** frame_descriptors;
  int frame_descriptors_mask;
  frame_descr * d;
  uintnat h;

  Assert(caml_frame_descriptor_table != Val_unit);
  frame_descriptors = Data_abstract_val(caml_frame_descriptor_table);
  frame_descriptors_mask = Wosize_val(caml_frame_descriptor_table) - 1;

  h = Hash_retaddr(pc, frame_descriptors_mask);
  while (1) {
    d = frame_descriptors[h];
    if (d == 0) return NULL; /* can happen if some code compiled without -g */
    if (d->retaddr == pc) break;
    h = (h+1) & frame_descriptors_mask;
  }
  return d;
}
