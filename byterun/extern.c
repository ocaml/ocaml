/* Structured output */

#include "fail.h"
#include "gc.h"
#include "intext.h"
#include "io.h"
#include "memory.h"
#include "misc.h"
#include "mlvalues.h"
#include "reverse.h"
#include "str.h"

/* To keep track of sharing in externed objects */

typedef unsigned long byteoffset_t;

struct extern_obj {
  value obj;
  byteoffset_t ofs;
};

static struct extern_obj * extern_table;
static asize_t extern_table_size;

#ifdef SIXTYFOUR
#define Hash(v) (((asize_t) ((v) >> 3)) % extern_table_size)
#else
#define Hash(v) (((asize_t) ((v) >> 2)) % extern_table_size)
#endif

static void alloc_extern_table()
{
  asize_t i;

  extern_table = (struct extern_obj *)
    stat_alloc(extern_table_size * sizeof(struct extern_obj));
  for (i = 0; i < extern_table_size; i++)
    extern_table[i].obj = 0;
}

static void resize_extern_table()
{
  asize_t oldsize;
  struct extern_obj * oldtable;
  asize_t i, h;

  oldsize = extern_table_size;
  oldtable = extern_table;
  extern_table_size = 2 * extern_table_size;
  alloc_extern_table();
  for (i = 0; i < oldsize; i++) {
    h = Hash(oldtable[i].obj);
    while (extern_table[h].obj != 0) {
      h++;
      if (h >= extern_table_size) h = 0;
    }
    extern_table[h].obj = oldtable[i].obj;
    extern_table[h].ofs = oldtable[i].ofs;
  }
  stat_free((char *) oldtable);
}

/* Write integers on a channel */

static void output8(chan, code, val)
     struct channel * chan;
     int code;
     long val;
{
  putch(chan, code); putch(chan, val);
}

static void output16(chan, code, val)
     struct channel * chan;
     int code;
     long val;
{
  putch(chan, code); putch(chan, val >> 8); putch(chan, val);
}

static void output32(chan, code, val)
     struct channel * chan;
     int code;
     long val;
{
  putch(chan, code);
  putch(chan, val >> 24); putch(chan, val >> 16);
  putch(chan, val >> 8); putch(chan, val);
}

#ifdef SIXTYFOUR
static void output64(chan, code, val)
     struct channel * chan;
     int code;
     long val;
{
  int i;
  putch(chan, code);
  for (i = 64 - 8; i >= 0; i -= 8) putch(chan, val >> i);
}
#endif

static byteoffset_t obj_counter;    /* Number of objects emitted so far */
static unsigned long size_32;  /* Size in words of 32-bit block for struct. */
static unsigned long size_64;  /* Size in words of 64-bit block for struct. */

static void emit_compact(chan, v)
     struct channel * chan;
     value v;
{
 tailcall:
  if (Is_long(v)) {
    long n = Long_val(v);
    if (n >= 0 && n < 0x40) {
      putch(chan, PREFIX_SMALL_INT + n);
    } else if (n >= -(1 << 7) && n < (1 << 7)) {
      output8(chan, CODE_INT8, n);
    } else if (n >= -(1 << 15) && n < (1 << 15)) {
      output16(chan, CODE_INT16, n);
#ifdef SIXTYFOUR
    } else if (n < -(1L << 31) || n >= (1L << 31)) {
      output64(chan, CODE_INT64, n);
#endif
    } else
      output32(chan, CODE_INT32, n);
  } else {
    header_t hd = Hd_val(v);
    tag_t tag = Tag_hd(hd);
    mlsize_t sz = Wosize_hd(hd);
      asize_t h;
    /* Atoms are treated specially for two reasons: they are not allocated
       in the externed block, and they are automatically shared. */
    if (sz == 0) {
      if (tag < 16) {
        putch(chan, PREFIX_SMALL_BLOCK + tag);
      } else {
        output32(chan, CODE_BLOCK32, hd);
      }
    } else {
      /* Check if already seen */
      if (2 * obj_counter >= extern_table_size) resize_extern_table();
      h = Hash(v);
      while (extern_table[h].obj != 0) {
        if (extern_table[h].obj == v) {
          byteoffset_t d = obj_counter - extern_table[h].ofs;
          if (d < 0x100) {
            output8(chan, CODE_SHARED8, d);
          } else if (d < 0x10000) {
            output16(chan, CODE_SHARED16, d);
          } else {
            output32(chan, CODE_SHARED32, d);
          }
          return;
        }
        h++;
        if (h >= extern_table_size) h = 0;
      }
      /* Not seen yet. Record the object and output its contents. */
      extern_table[h].obj = v;
      extern_table[h].ofs = obj_counter;
      obj_counter++;
      switch(tag) {
      case String_tag: {
        mlsize_t len = string_length(v);
        if (len < 0x20) {
          putch(chan, PREFIX_SMALL_STRING + len);
        } else if (len < 0x100) {
          output8(chan, CODE_STRING8, len);
        } else {
          output32(chan, CODE_STRING32, len);
        }
        putblock(chan, String_val(v), len);
        size_32 += 1 + (len + 4) / 4;
        size_64 += 1 + (len + 8) / 8;
        break;
      }
      case Double_tag: {
        double buffer;
        if (sizeof(double) != 8)
          invalid_argument("output_value: non-standard floats");
        putch(chan, CODE_DOUBLE_NATIVE);
        buffer = Double_val(v);
        putblock(chan, (char *) &buffer, 8);
        size_32 += 1 + sizeof(double) / 4;
        size_64 += 1 + sizeof(double) / 8;
        break;
      }
      case Abstract_tag:
      case Final_tag:
        invalid_argument("output_value: abstract value");
        break;
      case Closure_tag:
      case Infix_tag:
        invalid_argument("output_value: functional value");
        break;
      default: {
        mlsize_t i;
        if (tag < 16 && sz < 8) {
          putch(chan, PREFIX_SMALL_BLOCK + tag + (sz << 4));
        } else {
          output32(chan, CODE_BLOCK32, hd);
        }
        size_32 += 1 + sz;
        size_64 += 1 + sz;
        for (i = 0; i < sz - 1; i++) emit_compact(chan, Field(v, i));
        v = Field(v, i);
        goto tailcall;
      }
      }
    }
  }
}

value output_value(chan, v) /* ML */
     struct channel * chan;
     value v;
{
  value start_loc, final_loc;
  putword(chan, Compact_magic_number);
  start_loc = pos_out(chan);
  putword(chan, 0);
  putword(chan, 0);
  putword(chan, 0);
  extern_table_size = INITIAL_EXTERN_TABLE_SIZE;
  alloc_extern_table();
  obj_counter = 0;
  size_32 = 0;
  size_64 = 0;
  emit_compact(chan, v);
#ifdef SIXTYFOUR
  if (size_32 >= (1L << 32) || size_64 >= (1L << 32)) {
    /* The object is so big its size cannot be written in the header.
       Besides, some of the block sizes or string lengths or shared offsets
       it contains may have overflowed the 32 bits used to write them. */
    failwith("output_value: object too big");
  }
#endif
  final_loc = pos_out(chan);
  seek_out(chan, start_loc);
  putword(chan, obj_counter);
  putword(chan, size_32);
  putword(chan, size_64);
  seek_out(chan, final_loc);
  stat_free((char *) extern_table);
  return Val_unit;
}
