#include "config.h"
#include "memory.h"
#include "addrmap.h"

#define Is_power_of_2(x) (((x) & ((x) - 1)) == 0)

static const value INVALID_KEY = (value)0;

static uintnat pos_initial(struct addrmap* t, value key) 
{
  uintnat pos = (uintnat)key;
  pos *= 0xcc9e2d51;
  pos ^= (pos >> 17);

  Assert(Is_power_of_2(t->size));
  return pos & (t->size - 1);
}

static uintnat pos_next(struct addrmap* t, uintnat pos)
{
  return (pos + 1) & (t->size - 1);
}

value caml_addrmap_lookup(struct addrmap* t, value key)
{
  Assert(Is_block(key));
  Assert(t->entries);
  
  uintnat pos;
  for (pos = pos_initial(t, key); ; pos = pos_next(t, pos)) {
    Assert(t->entries[pos].key != INVALID_KEY);
    if (t->entries[pos].key == key)
      return t->entries[pos].value;
  }
}

static void addrmap_alloc(struct addrmap* t, uintnat sz)
{
  uintnat i;
  Assert(sz > 0 && (sz & (sz - 1)) == 0); /* sz must be a power of 2 */
  t->entries = caml_stat_alloc(sizeof(struct addrmap_entry) * sz);
  t->size = sz;
  for (i = 0; i < sz; i++) {
    t->entries[i].key = INVALID_KEY;
    t->entries[i].value = ADDRMAP_NOT_PRESENT;
  }
}

void caml_addrmap_clear(struct addrmap* t) {
  caml_stat_free(t->entries);
  t->entries = 0;
  t->size = 0;
}



#define MAX_CHAIN 100

value* caml_addrmap_insert_pos(struct addrmap* t, value key) {
  uintnat i, pos;
  Assert(Is_block(key));
  if (!t->entries) {
    /* first call, initialise table with a small initial size */
    addrmap_alloc(t, 256);
  }
  for (i = 0, pos = pos_initial(t, key); 
       i < MAX_CHAIN; 
       i++,   pos = pos_next(t, pos)) {
    if (t->entries[pos].key == INVALID_KEY) {
      t->entries[pos].key = key;
    }
    if (t->entries[pos].key == key) {
      return &t->entries[pos].value;
    }
  }
  /* failed to insert, rehash and try again */
  struct addrmap_entry* old_table = t->entries;
  uintnat old_size = t->size;
  addrmap_alloc(t, old_size * 2);
  for (i = 0; i < old_size; i++) {
    if (old_table[i].key != INVALID_KEY) {
      value* p = caml_addrmap_insert_pos(t, old_table[i].key);
      Assert(*p == ADDRMAP_NOT_PRESENT);
      *p = old_table[i].value;
    }
  }
  caml_stat_free(old_table);
  return caml_addrmap_insert_pos(t, key);
}
