/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*      KC Sivaramakrishnan, Indian Institute of Technology, Madras       */
/*                Stephen Dolan, University of Cambridge                  */
/*                                                                        */
/*   Copyright 2020 Indian Institute of Technology, Madras                */
/*   Copyright 2020 University of Cambridge                               */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

#include "caml/config.h"
#include "caml/memory.h"
#include "caml/addrmap.h"
#include "caml/startup_aux.h"

int addrmap_page_table_initialize(struct addrmap_page_table *t, mlsize_t count)
{
  uintnat pagesize = Page(count * 4);

  t->size  = 1;
  t->shift = 8 * sizeof(uintnat);

  /* Aim for initial load factor between 1/4 and 1/2 */
  while (t->size < 2 * pagesize) {
    t->size <<= 1;
    t->shift -= 1;
  }
  t->mask      = t->size - 1;
  t->occupancy = 0;

  /* Allocate for entries */
  mlsize_t sz = t->size;
  CAMLassert(sz > 0 && (sz & (sz - 1)) == 0); /* sz must be a power of 2 */
  t->entries = caml_stat_alloc(sizeof(struct addrmap_entry) * sz);
  for (int i = 0; i < sz; i++) {
    t->entries[i].key   = ADDRMAP_INVALID_KEY;
    t->entries[i].value = ADDRMAP_NOT_PRESENT;
  }

  if (t->entries == NULL)
    return -1;
  else
    return 0;
}

void display_addrmap(struct addrmap_page_table* t)
{
  mlsize_t i;

  printf("Display addrmap page table entries\n");
  printf("size       = %ld\n", t->size);
  printf("occupancy  = %ld\n", t->occupancy);
  for (i = 0; i < t->size; i++)
    printf("addrmap_page_table.entries[%ld] = %ld:%ld\n", i, t->entries[i].key, t->entries[i].value);
  printf("\n\n");
}

int addrmap_page_table_resize(struct addrmap_page_table* t)
{
  struct addrmap_entry* new_entries;
  uintnat i, h, new_size, new_shift, new_mask;

  caml_gc_message (0x08, "Growing page table to %"
                   ARCH_INTNAT_PRINTF_FORMAT "u entries\n",
                   t->size);

  new_size    = t->size * 2;
  new_shift   = t->shift - 1;
  new_mask    = new_size - 1;
  new_entries = caml_stat_alloc(sizeof(struct addrmap_entry) * new_size);

  for (int i = 0; i < new_size; i++) {
    new_entries[i].key   = ADDRMAP_INVALID_KEY;
    new_entries[i].value = ADDRMAP_NOT_PRESENT;
  }

  for (i = 0; i < t->size; i++) {
    struct addrmap_entry e = t->entries[i];
    if (e.key != ADDRMAP_INVALID_KEY) {
      h = Hash(Page(e.key), new_shift);

      if (new_entries[h].key == ADDRMAP_INVALID_KEY) {
        new_entries[h].key   = t->entries[i].key;
        new_entries[h].value = t->entries[i].value;
      }
      else {
        while (1) {
          h = (h + 1) & new_mask;
          if (new_entries[h].key == ADDRMAP_INVALID_KEY) {
            new_entries[h].key   = t->entries[i].key;
            new_entries[h].value = t->entries[i].value;
            break;
          }
        }
      }
    }
  }

  t->size  = new_size;
  t->shift = new_shift;
  t->mask  = new_mask;

  caml_stat_free(t->entries);
  t->entries = new_entries;
  return 0;
}

value* addrmap_page_table_lookup(struct addrmap_page_table* t, value key)
{
  uintnat h; /* e, i; */

  h = Hash(Page(key), t->shift);
  /* The first hit is almost always successful, so optimize for this case */
  if (t->entries[h].key == ADDRMAP_INVALID_KEY) {
      t->entries[h].key = key;
      t->occupancy++;
    }
  if (t->entries[h].key == key) {
    return &t->entries[h].value;
  }

  while (1) {
    h = (h + 1) & t->mask;
    if (t->entries[h].key == ADDRMAP_INVALID_KEY) {
      t->entries[h].key = key;
      t->occupancy++;
    }
    if (t->entries[h].key == key) {
      return &t->entries[h].value;
    }
  }

  return NULL;
}

void caml_addrmap_clear(struct addrmap_page_table* t) {
  caml_stat_free(t->entries);
  t->entries   = NULL;
  t->occupancy = 0;
  t->mask      = 0;
  t->shift     = 0;
  t->size      = 0;
}

void caml_addrmap_initialize(struct addrmap_page_table* t) {
  addrmap_page_table_initialize(t, caml_init_intern_addrmap_size);
}

value* caml_addrmap_insert_pos(struct addrmap_page_table* t, value key) {
  CAMLassert(Is_block(key));

  /* Resize to keep load factor around 0.9 */
  if (t->occupancy > 0.9 * t->size) {

    if (addrmap_page_table_resize(t) != 0) {
      return NULL;
    }
  }
  return addrmap_page_table_lookup(t, key);
}
