/* Structured input, compact format */

#include "fail.h"
#include "gc.h"
#include "intext.h"
#include "io.h"
#include "memory.h"
#include "mlvalues.h"
#include "misc.h"
#include "reverse.h"

static header_t * intern_ptr;
static asize_t obj_counter;
static value * intern_obj_table;
static unsigned int intern_color;
static header_t intern_header;
static value intern_block;

#define Sign_extend_shift ((sizeof(long) - 1) * 8)
#define Sign_extend(x) (((long)(x) << Sign_extend_shift) >> Sign_extend_shift)

static long input8u(chan)
     struct channel * chan;
{
  return getch(chan);
}

static long input8s(chan)
     struct channel * chan;
{
  long b1 = getch(chan);
  return Sign_extend(b1);
}

static long input16u(chan)
     struct channel * chan;
{
  long b1 = getch(chan);
  long b2 = getch(chan);
  return (b1 << 8) + b2;
}

static long input16s(chan)
     struct channel * chan;
{
  long b1 = getch(chan);
  long b2 = getch(chan);
  return (Sign_extend(b1) << 8) + b2;
}

static long input32u(chan)
     struct channel * chan;
{
  long b1 = getch(chan);
  long b2 = getch(chan);
  long b3 = getch(chan);
  long b4 = getch(chan);
  return (b1 << 24) + (b2 << 16) + (b3 << 8) + b4;
}

static long input32s(chan)
     struct channel * chan;
{
  long b1 = getch(chan);
  long b2 = getch(chan);
  long b3 = getch(chan);
  long b4 = getch(chan);
  return (Sign_extend(b1) << 24) + (b2 << 16) + (b3 << 8) + b4;
}

#ifdef SIXTYFOUR
static long input64s(chan)
     struct channel * chan;
{
  long res;
  int i;
  res = 0;
  for (i = 0; i < 8; i++) res = (res << 8) + getch(chan);
  return res;
}
#endif

static void read_compact(chan, dest)
     struct channel * chan;
     value * dest;
{
  unsigned int code;
  tag_t tag;
  mlsize_t size, len, ofs_ind;
  value v;
  asize_t ofs;
  header_t header;

 tailcall:
  code = getch(chan);
  if (code >= PREFIX_SMALL_INT) {
    if (code >= PREFIX_SMALL_BLOCK) {
      /* Small block */
      tag = code & 0xF;
      size = (code >> 4) & 0x7;
    read_block:
      if (size == 0) {
        v = Atom(tag);
      } else {
        v = Val_hp(intern_ptr);
        *dest = v;
        intern_obj_table[obj_counter++] = v;
        dest = (value *) (intern_ptr + 1);
        *intern_ptr = Make_header(size, tag, intern_color);
        intern_ptr += 1 + size;
        for(/*nothing*/; size > 1; size--, dest++)
          read_compact(chan, dest);
        goto tailcall;
      }
    } else {
      /* Small integer */
      v = Val_int(code & 0x3F);
    }
  } else {
    if (code >= PREFIX_SMALL_STRING) {
      /* Small string */
      len = (code & 0x1F);
    read_string:
      size = (len + sizeof(value)) / sizeof(value);
      v = Val_hp(intern_ptr);
      intern_obj_table[obj_counter++] = v;
      *intern_ptr = Make_header(size, String_tag, intern_color);
      intern_ptr += 1 + size;
      Field(v, size - 1) = 0;
      ofs_ind = Bsize_wsize(size) - 1;
      Byte(v, ofs_ind) = ofs_ind - len;
      really_getblock(chan, String_val(v), len);
    } else {
      switch(code) {
      case CODE_INT8:
        v = Val_long(input8s(chan));
        break;
      case CODE_INT16:
        v = Val_long(input16s(chan));
        break;
      case CODE_INT32:
        v = Val_long(input32s(chan));
        break;
      case CODE_INT64:
#ifdef SIXTYFOUR
        v = Val_long(input64s(chan));
        break;
#else
        stat_free((char *) intern_obj_table);
        Hd_val(intern_block) = intern_header; /* Don't confuse the GC */
        failwith("input_value: integer too large");
        break;
#endif
      case CODE_SHARED8:
        ofs = input8u(chan);
      read_shared:
        Assert(ofs > 0 && ofs <= obj_counter); 
        v = intern_obj_table[obj_counter - ofs];
        break;
      case CODE_SHARED16:
        ofs = input16u(chan);
        goto read_shared;
      case CODE_SHARED32:
        ofs = input32u(chan);
        goto read_shared;
      case CODE_BLOCK32:
        header = (header_t) input32u(chan);
        tag = Tag_hd(header);
        size = Wosize_hd(header);
        goto read_block;
      case CODE_STRING8:
        len = input8u(chan);
        goto read_string;
      case CODE_STRING32:
        len = input32u(chan);
        goto read_string;
      case CODE_DOUBLE_LITTLE:
      case CODE_DOUBLE_BIG:
        if (sizeof(double) != 8) {
          stat_free((char *) intern_obj_table);
          Hd_val(intern_block) = intern_header; /* Don't confuse the GC */
          invalid_argument("input_value: non-standard floats");
        }
        v = Val_hp(intern_ptr);
        intern_obj_table[obj_counter++] = v;
        *intern_ptr = Make_header(Double_wosize, Double_tag, intern_color);
        intern_ptr += 1 + Double_wosize;
        really_getblock(chan, (char *) v, 8);
        if (code != CODE_DOUBLE_NATIVE) Reverse_double(v);
        break;
      }
    }
  }
  *dest = v;
}

value input_value(chan)         /* ML */
     struct channel * chan;
{
  uint32 magic;
  mlsize_t num_objects, size_32, size_64, whsize;
  value res;

  magic = getword(chan);
  if (magic != Compact_magic_number) failwith("input_value: bad object");
  num_objects = getword(chan);
  size_32 = getword(chan);
  size_64 = getword(chan);
#ifdef SIXTYFOUR
  whsize = size_64;
#else
  whsize = size_32;
#endif
  if (whsize == 0) {
    read_compact(chan, &res);
  } else {
    if (Wosize_whsize(whsize) > Max_wosize)
      failwith("intern: structure too big");
    intern_block = alloc_shr(Wosize_whsize(whsize), String_tag);
    intern_header = Hd_val(intern_block);
    intern_color = Color_hd(intern_header);
    Assert (intern_color == White || intern_color == Black);
    intern_ptr = (header_t *) Hp_val(intern_block);
    obj_counter = 0;
    intern_obj_table = (value *) stat_alloc(num_objects * sizeof(value));
    read_compact(chan, &res);
    stat_free((char *) intern_obj_table);
  }
  return res;
}
