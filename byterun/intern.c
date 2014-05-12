/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* Structured input, compact format */

/* The interface of this file is "intext.h" */

#include <string.h>
#include <stdio.h>
#include "alloc.h"
#include "callback.h"
#include "custom.h"
#include "fail.h"
#include "gc.h"
#include "intext.h"
#include "io.h"
#include "md5.h"
#include "memory.h"
#include "mlvalues.h"
#include "misc.h"
#include "reverse.h"

static unsigned char * intern_src;
/* Reading pointer in block holding input data. */

static unsigned char * intern_input;
/* Pointer to beginning of block holding input data.
   Meaningful only if intern_input_malloced = 1. */

static int intern_input_malloced;
/* 1 if intern_input was allocated by caml_stat_alloc()
   and needs caml_stat_free() on error, 0 otherwise. */

static char * intern_resolve_code_pointer(unsigned char digest[16],
                                          asize_t offset);
static void intern_bad_code_pointer(unsigned char digest[16]) Noreturn;

#define Sign_extend_shift ((sizeof(intnat) - 1) * 8)
#define Sign_extend(x) (((intnat)(x) << Sign_extend_shift) >> Sign_extend_shift)

#define read8u() (*intern_src++)
#define read8s() Sign_extend(*intern_src++)
#define read16u() \
  (intern_src += 2, \
   (intern_src[-2] << 8) + intern_src[-1])
#define read16s() \
  (intern_src += 2, \
   (Sign_extend(intern_src[-2]) << 8) + intern_src[-1])
#define read32u() \
  (intern_src += 4, \
   ((uintnat)(intern_src[-4]) << 24) + (intern_src[-3] << 16) + \
   (intern_src[-2] << 8) + intern_src[-1])
#define read32s() \
  (intern_src += 4, \
   (Sign_extend(intern_src[-4]) << 24) + (intern_src[-3] << 16) + \
   (intern_src[-2] << 8) + intern_src[-1])

#ifdef ARCH_SIXTYFOUR
static intnat read64s(void)
{
  intnat res;
  int i;
  res = 0;
  for (i = 0; i < 8; i++) res = (res << 8) + intern_src[i];
  intern_src += 8;
  return res;
}
#endif

#define readblock(dest,len) \
  (memmove((dest), intern_src, (len)), intern_src += (len))

struct intern_stack;
static void intern_cleanup(struct intern_stack*);

static void readfloat(double * dest, unsigned int code)
{
  if (sizeof(double) != 8) {
    intern_cleanup(0); /* !! LEAK */
    caml_invalid_argument("input_value: non-standard floats");
  }
  readblock((char *) dest, 8);
  /* Fix up endianness, if needed */
#if ARCH_FLOAT_ENDIANNESS == 0x76543210
  /* Host is big-endian; fix up if data read is little-endian */
  if (code != CODE_DOUBLE_BIG) Reverse_64(dest, dest);
#elif ARCH_FLOAT_ENDIANNESS == 0x01234567
  /* Host is little-endian; fix up if data read is big-endian */
  if (code != CODE_DOUBLE_LITTLE) Reverse_64(dest, dest);
#else
  /* Host is neither big nor little; permute as appropriate */
  if (code == CODE_DOUBLE_LITTLE)
    Permute_64(dest, ARCH_FLOAT_ENDIANNESS, dest, 0x01234567)
  else
    Permute_64(dest, ARCH_FLOAT_ENDIANNESS, dest, 0x76543210);
#endif
}

static void readfloats(double * dest, mlsize_t len, unsigned int code)
{
  mlsize_t i;
  if (sizeof(double) != 8) {
    intern_cleanup(0); /* !! LEAK */
    caml_invalid_argument("input_value: non-standard floats");
  }
  readblock((char *) dest, len * 8);
  /* Fix up endianness, if needed */
#if ARCH_FLOAT_ENDIANNESS == 0x76543210
  /* Host is big-endian; fix up if data read is little-endian */
  if (code != CODE_DOUBLE_ARRAY8_BIG &&
      code != CODE_DOUBLE_ARRAY32_BIG) {
    for (i = 0; i < len; i++) Reverse_64(dest + i, dest + i);
  }
#elif ARCH_FLOAT_ENDIANNESS == 0x01234567
  /* Host is little-endian; fix up if data read is big-endian */
  if (code != CODE_DOUBLE_ARRAY8_LITTLE &&
      code != CODE_DOUBLE_ARRAY32_LITTLE) {
    for (i = 0; i < len; i++) Reverse_64(dest + i, dest + i);
  }
#else
  /* Host is neither big nor little; permute as appropriate */
  if (code == CODE_DOUBLE_ARRAY8_LITTLE ||
      code == CODE_DOUBLE_ARRAY32_LITTLE) {
    for (i = 0; i < len; i++)
      Permute_64(dest + i, ARCH_FLOAT_ENDIANNESS, dest + i, 0x01234567);
  } else {
    for (i = 0; i < len; i++)
      Permute_64(dest + i, ARCH_FLOAT_ENDIANNESS, dest + i, 0x76543210);
  }
#endif
}


enum {
  OReadItems, /* read arg items and store them in dest[0], dest[1], ... */
  OFreshOID,  /* generate a fresh OID and store it in *dest */
  OShift      /* offset *dest by arg */
};


#define STACK_NFIELDS 4
#define STACK_VAL(s, i)   (s)[i][0] /* the value being read */
#define STACK_FIELD(s, i) (s)[i][1] /* the next field to be read */
#define STACK_OP(s, i)    (s)[i][2] /* the operation */
#define STACK_ARG(s, i)   (s)[i][3] /* if op == OReadItems, the length */


#define INTERN_STACK_INIT_SIZE 64
#define INTERN_STACK_MAX_SIZE (1024*1024*100)
typedef value intern_stack_item[STACK_NFIELDS];
struct intern_stack {
  struct caml__roots_block caml__roots_stack;

  intern_stack_item first_vals[INTERN_STACK_INIT_SIZE];

  intern_stack_item* curr_vals;
  int len;
  int sp;
};

static void stack_init(struct intern_stack* s) {
  int i, j;
  for (i = 0; i < INTERN_STACK_INIT_SIZE; i++) {
    for (j = 0; j < STACK_NFIELDS; j++) {
      s->first_vals[i][j] = Val_unit;
    }
  }
  s->curr_vals = s->first_vals;
  s->len = INTERN_STACK_INIT_SIZE;
  s->sp = 0;

  /* Set up GC roots */
  s->caml__roots_stack.next = caml_local_roots;
  caml_local_roots = &s->caml__roots_stack;
  s->caml__roots_stack.mutexes = 0;
  s->caml__roots_stack.ntables = 1;
  s->caml__roots_stack.nitems = INTERN_STACK_INIT_SIZE * STACK_NFIELDS;
  s->caml__roots_stack.tables[0] = (value*)s->curr_vals;
}

static int stack_is_empty(struct intern_stack* s) {
  return s->sp == 0;
}

static void stack_free(struct intern_stack* s) {
  /* free any memory allocated */
  if (s->curr_vals != s->first_vals) caml_stat_free(s->curr_vals);
}

static void stack_realloc(struct intern_stack* s, value save) {
  CAMLparam1(save);
  /* reallocate stack */
  caml_gc_log("stack realloc");
  int i;
  int new_len = s->len * 2;
  if (new_len >= INTERN_STACK_MAX_SIZE) {
    caml_gc_log ("Stack overflow in un-marshaling value");
    stack_free(s);
    caml_raise_out_of_memory();
  }

  intern_stack_item* new_vals = caml_stat_alloc(new_len * STACK_NFIELDS * sizeof(value));
  
  for (i = 0; i < s->sp; i++) {
    STACK_VAL(new_vals, i) = STACK_VAL(s->curr_vals, i);
    STACK_FIELD(new_vals, i) = STACK_FIELD(s->curr_vals, i);
    STACK_OP(new_vals, i) = STACK_OP(s->curr_vals, i);
    STACK_ARG(new_vals, i) = STACK_ARG(s->curr_vals, i);
  }

  if (s->curr_vals != s->first_vals) caml_stat_free(s->curr_vals);
  
  /* register GC root */
  s->curr_vals = new_vals;
  s->len = new_len;
  s->caml__roots_stack.nitems = new_len * STACK_NFIELDS;
  s->caml__roots_stack.tables[0] = (value*)new_vals;
  CAMLreturn0;
}

static void stack_push(struct intern_stack* s, value v, int field, int op, intnat arg) {
  if (Is_block(v)) {
    Assert(field < Wosize_hd(Hd_val(v)));
  }
  if (s->sp == s->len - 1) {
    stack_realloc(s, v);
  }
  STACK_VAL(s->curr_vals, s->sp) = v;
  STACK_FIELD(s->curr_vals, s->sp) = Val_int(field);
  STACK_OP(s->curr_vals, s->sp) = Val_int(op);
  STACK_ARG(s->curr_vals, s->sp) = Val_long(arg);
  s->sp++;
}

static void stack_pop(struct intern_stack* s) {
  Assert(!stack_is_empty(s));
  s->sp--;
}

static value stack_curr_val(struct intern_stack* s) {
  Assert(!stack_is_empty(s));
  return STACK_VAL(s->curr_vals, s->sp - 1);
}

static int stack_curr_op(struct intern_stack* s) {
  Assert(!stack_is_empty(s));
  return Int_val(STACK_OP(s->curr_vals, s->sp - 1));
}

static int stack_curr_field(struct intern_stack* s) {
  Assert(!stack_is_empty(s));
  return Int_val(STACK_FIELD(s->curr_vals, s->sp - 1));
}

static intnat stack_curr_arg(struct intern_stack* s) {
  Assert(!stack_is_empty(s));
  return Long_val(STACK_ARG(s->curr_vals, s->sp - 1));
}

static void stack_advance_field(struct intern_stack* s) {
  Assert(!stack_is_empty(s));
  int field = Int_val(STACK_FIELD(s->curr_vals, s->sp - 1));
  field++;
  STACK_FIELD(s->curr_vals, s->sp - 1) = Val_int(field);
  if (field == Int_val(STACK_ARG(s->curr_vals, s->sp - 1))) {
    stack_pop(s);
  }
}

static void stack_push_items(struct intern_stack* s, value dest, int n) {
  if (n > 0) {
    stack_push(s, dest, 0, OReadItems, n);
  }
}


static void intern_cleanup(struct intern_stack* s)
{
  if (s) stack_free(s);
  /* !!
  if (intern_input_malloced) caml_stat_free(intern_input);
  */
}


static value intern_rec(mlsize_t whsize, mlsize_t num_objects)
{
  int first = 1;
  int curr_field;
  unsigned int code;
  tag_t tag;
  mlsize_t size, len;
  asize_t ofs;
  header_t header;
  unsigned char digest[16];
  struct custom_operations * ops;
  char * codeptr;
  struct intern_stack S;
  asize_t obj_counter = 0; /* Count how many objects seen so far */
  int use_intern_table;

  CAMLparam0();
  CAMLlocal4(v,                 /* the current object being read */
             dest,              /* the object into which v will be inserted */
             result,            /* the eventual result */
             intern_obj_table); /* object table for storing shared objects */
  stack_init(&S);

  use_intern_table = whsize > 0 && num_objects > 0;
  if (use_intern_table) {
    intern_obj_table = caml_alloc(num_objects, 0);
  }

  /* Initially let's try to read the first object from the stream */
  stack_push(&S, Val_unit, 0, OReadItems, 1);

  /* The un-marshaler loop, the recursion is unrolled */
  while (!stack_is_empty(&S)) {
    
    /* Interpret next item on the stack */
    dest = stack_curr_val(&S);
    curr_field = stack_curr_field(&S);
    if (!first) {
      Assert (0 <= curr_field && curr_field < Wosize_hd(Hd_val(dest)));
    }

    switch (stack_curr_op(&S)) {
    case OFreshOID:
      /* Refresh the object ID */
      /* but do not do it for predefined exception slots */
      if (Int_val(Field(dest, 1)) >= 0)
        caml_set_oo_id(dest);
      /* Pop item and iterate */
      stack_pop(&S);
      break;
    case OShift:
      caml_failwith("shift op");
      /* Shift value by an offset */
      /* !! */
      /* *dest += sp->arg; */
      /* Pop item and iterate */
      /* sp--; */
      break;
    case OReadItems:
      /* Pop item */
      stack_advance_field(&S);
      /* Read a value and set v to this value */
      code = read8u();
      if (code >= PREFIX_SMALL_INT) {
        if (code >= PREFIX_SMALL_BLOCK) {
          /* Small block */
          tag = code & 0xF;
          size = (code >> 4) & 0x7;
        read_block:
          if (size == 0) {
            v = Atom(tag);
          } else {
            Assert(tag != Closure_tag);
            Assert(tag != Infix_tag);
            v = caml_alloc(size, tag);
            if (use_intern_table) caml_modify_field(intern_obj_table, obj_counter++, v);
            /* For objects, we need to freshen the oid */
            if (tag == Object_tag) {
              Assert(size >= 2);
              /* Request to read rest of the elements of the block */
              if (size > 2) stack_push(&S, v, 2, OReadItems, size - 2);
              /* Request freshing OID */
              stack_push(&S, v, 1, OFreshOID, 1);
              /* Finally read first two block elements: method table and old OID */
              stack_push(&S, v, 0, OReadItems, 2);
            } else {
              /* If it's not an object then read the contents of the block */
              stack_push_items(&S, v, size);
            }
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
          v = caml_alloc_string(len);
          if (use_intern_table) caml_modify_field(intern_obj_table, obj_counter++, v);
          readblock(String_val(v), len);
        } else {
          switch(code) {
          case CODE_INT8:
            v = Val_long(read8s());
            break;
          case CODE_INT16:
            v = Val_long(read16s());
            break;
          case CODE_INT32:
            v = Val_long(read32s());
            break;
          case CODE_INT64:
#ifdef ARCH_SIXTYFOUR
            v = Val_long(read64s());
            break;
#else
            intern_cleanup(&S);
            caml_failwith("input_value: integer too large");
            break;
#endif
          case CODE_SHARED8:
            ofs = read8u();
          read_shared:
            Assert (ofs > 0);
            Assert (ofs <= obj_counter);
            Assert (use_intern_table);
            v = Field(intern_obj_table, obj_counter - ofs);
            break;
          case CODE_SHARED16:
            ofs = read16u();
            goto read_shared;
          case CODE_SHARED32:
            ofs = read32u();
            goto read_shared;
          case CODE_BLOCK32:
            header = (header_t) read32u();
            tag = Tag_hd(header);
            size = Wosize_hd(header);
            goto read_block;
          case CODE_BLOCK64:
#ifdef ARCH_SIXTYFOUR
            header = (header_t) read64s();
            tag = Tag_hd(header);
            size = Wosize_hd(header);
            goto read_block;
#else
            intern_cleanup(&S);
            caml_failwith("input_value: data block too large");
            break;
#endif
          case CODE_STRING8:
            len = read8u();
            goto read_string;
          case CODE_STRING32:
            len = read32u();
            goto read_string;
          case CODE_DOUBLE_LITTLE:
          case CODE_DOUBLE_BIG:
            v = caml_alloc(Double_wosize, Double_tag);
            if (use_intern_table) caml_modify_field(intern_obj_table, obj_counter++, v);
            readfloat((double *) v, code);
            break;
          case CODE_DOUBLE_ARRAY8_LITTLE:
          case CODE_DOUBLE_ARRAY8_BIG:
            len = read8u();
          read_double_array:
            v = caml_alloc(len * Double_wosize, Double_array_tag);
            if (use_intern_table) caml_modify_field(intern_obj_table, obj_counter++, v);
            readfloats((double *) v, len, code);
            break;
          case CODE_DOUBLE_ARRAY32_LITTLE:
          case CODE_DOUBLE_ARRAY32_BIG:
            len = read32u();
            goto read_double_array;
          case CODE_CODEPOINTER:
            caml_failwith("wtfff");
            ofs = read32u();
            readblock(digest, 16);
            codeptr = intern_resolve_code_pointer(digest, ofs); /* !! */
            if (codeptr != NULL) {
              v = (value) codeptr;
            } else {
              int found_placeholder;
              value function_placeholder =
                caml_get_named_value ("Debugger.function_placeholder", &found_placeholder);
              if (found_placeholder) {
                v = function_placeholder;
              } else {
                intern_cleanup(&S);
                intern_bad_code_pointer(digest);
              }
            }
            break;
          case CODE_INFIXPOINTER:
            caml_failwith("wtf");
#if 0
            ofs = read32u();
            /* Read a value to *dest, then offset *dest by ofs */
            PushItem();
            sp->dest = dest;
            sp->op = OShift;
            sp->arg = ofs;
            ReadItems(dest, 1);
            continue;  /* with next iteration of main loop, skipping *dest = v */
#endif
          case CODE_CUSTOM:
            ops = caml_find_custom_operations((char *) intern_src);
            if (ops == NULL) {
              intern_cleanup(&S);
              caml_failwith("input_value: unknown custom block identifier");
            }
            while (*intern_src++ != 0) /*nothing*/;  /*skip identifier*/
            v = ops->deserialize();
            if (use_intern_table) caml_modify_field(intern_obj_table, obj_counter++, v);
            break;
          default:
            intern_cleanup(&S);
            caml_failwith("input_value: ill-formed message");
          }
        }
      }
      /* end of case OReadItems */
      if (first) {
        result = v;
        first = 0;
      } else {
        caml_modify_field(dest, curr_field, v);
      }
      break;
    default:
      Assert(0);
    }
  }
  stack_free(&S);
  CAMLreturn(result);
}

value caml_input_val(struct channel *chan)
{
  uint32 magic;
  mlsize_t block_len, num_objects, whsize;
  char * block;
  value res;

  if (! caml_channel_binary_mode(chan))
    caml_failwith("input_value: not a binary channel");
  magic = caml_getword(chan);
  if (magic != Intext_magic_number) caml_failwith("input_value: bad object");
  block_len = caml_getword(chan);
  num_objects = caml_getword(chan);
#ifdef ARCH_SIXTYFOUR
  caml_getword(chan); /* skip size_32 */
  whsize = caml_getword(chan);
#else
  whsize = caml_getword(chan);
  caml_getword(chan); /* skip size_64 */
#endif
  /* Read block from channel */
  block = caml_stat_alloc(block_len);
  /* During [caml_really_getblock], concurrent [caml_input_val] operations
     can take place (via signal handlers or context switching in systhreads),
     and [intern_input] may change.  So, wait until [caml_really_getblock]
     is over before using [intern_input] and the other global vars. */
  if (caml_really_getblock(chan, block, block_len) == 0) {
    caml_stat_free(block);
    caml_failwith("input_value: truncated object");
  }
  intern_input = (unsigned char *) block;
  intern_input_malloced = 1;
  intern_src = intern_input;
  /* Fill it in */
  res = intern_rec(whsize, num_objects);
  /* Free everything */
  /* !! 
  caml_stat_free(intern_input);
  */
  res = caml_check_urgent_gc(res);
  return res;
}

CAMLprim value caml_input_value(value vchan)
{
  CAMLparam1 (vchan);
  struct channel * chan = Channel(vchan);
  CAMLlocal1 (res);

  With_mutex(&chan->mutex) {
    res = caml_input_val(chan);
  }
  CAMLreturn (res);
}

CAMLexport value caml_input_val_from_string(value str, intnat ofs)
{
  CAMLparam1 (str);
  mlsize_t num_objects, whsize;
  CAMLlocal1 (obj);

  caml_failwith("lolwat");

  intern_src = &Byte_u(str, ofs + 2*4);
  intern_input_malloced = 0;
  num_objects = read32u();
#ifdef ARCH_SIXTYFOUR
  intern_src += 4;  /* skip size_32 */
  whsize = read32u();
#else
  whsize = read32u();
  intern_src += 4;  /* skip size_64 */
#endif
  /* Allocate result */
  intern_src = &Byte_u(str, ofs + 5*4); /* If a GC occurred */ /* !!!!! src ptr broken */
  /* Fill it in */
  obj = intern_rec(whsize, num_objects);
  /* Free everything !! */
  CAMLreturn (caml_check_urgent_gc(obj));
}

CAMLprim value caml_input_value_from_string(value str, value ofs)
{
  return caml_input_val_from_string(str, Long_val(ofs));
}

static value input_val_from_block(void)
{
  mlsize_t num_objects, whsize;
  value obj;

  num_objects = read32u();
#ifdef ARCH_SIXTYFOUR
  intern_src += 4;  /* skip size_32 */
  whsize = read32u();
#else
  whsize = read32u();
  intern_src += 4;  /* skip size_64 */
#endif
  /* Fill it in */
  obj = intern_rec(whsize, num_objects);
  /* Free internal data structures */
  return caml_check_urgent_gc(obj);
}

CAMLexport value caml_input_value_from_malloc(char * data, intnat ofs)
{
  uint32 magic;
  value obj;

  intern_input = (unsigned char *) data;
  intern_src = intern_input + ofs;
  intern_input_malloced = 1;
  magic = read32u();
  if (magic != Intext_magic_number)
    caml_failwith("input_value_from_malloc: bad object");
  intern_src += 4;  /* Skip block_len */
  obj = input_val_from_block();
  /* Free the input */
  caml_stat_free(intern_input);
  return obj;
}

CAMLexport value caml_input_value_from_block(char * data, intnat len)
{
  uint32 magic;
  mlsize_t block_len;
  value obj;

  intern_input = (unsigned char *) data;
  intern_src = intern_input;
  intern_input_malloced = 0;
  magic = read32u();
  if (magic != Intext_magic_number)
    caml_failwith("input_value_from_block: bad object");
  block_len = read32u();
  if (5*4 + block_len > len)
    caml_failwith("input_value_from_block: bad block length");
  obj = input_val_from_block();
  return obj;
}

CAMLprim value caml_marshal_data_size(value buff, value ofs)
{
  uint32 magic;
  mlsize_t block_len;

  intern_src = &Byte_u(buff, Long_val(ofs));
  intern_input_malloced = 0;
  magic = read32u();
  if (magic != Intext_magic_number){
    caml_failwith("Marshal.data_size: bad object");
  }
  block_len = read32u();
  return Val_long(block_len);
}

/* Resolution of code pointers */

static char * intern_resolve_code_pointer(unsigned char digest[16],
                                          asize_t offset)
{
  int i;
  for (i = caml_code_fragments_table.size - 1; i >= 0; i--) {
    struct code_fragment * cf = caml_code_fragments_table.contents[i];
    if (! cf->digest_computed) {
      caml_md5_block(cf->digest, cf->code_start, cf->code_end - cf->code_start);
      cf->digest_computed = 1;
    }
    if (memcmp(digest, cf->digest, 16) == 0) {
      if (cf->code_start + offset < cf->code_end)
        return cf->code_start + offset;
      else
        return NULL;
    }
  }
  return NULL;
}

static void intern_bad_code_pointer(unsigned char digest[16])
{
  char msg[256];
  sprintf(msg, "input_value: unknown code module "
               "%02X%02X%02X%02X%02X%02X%02X%02X"
               "%02X%02X%02X%02X%02X%02X%02X%02X",
          digest[0], digest[1], digest[2], digest[3],
          digest[4], digest[5], digest[6], digest[7],
          digest[8], digest[9], digest[10], digest[11],
          digest[12], digest[13], digest[14], digest[15]);
  caml_failwith(msg);
}

/* Functions for writing user-defined marshallers */

CAMLexport int caml_deserialize_uint_1(void)
{
  return read8u();
}

CAMLexport int caml_deserialize_sint_1(void)
{
  return read8s();
}

CAMLexport int caml_deserialize_uint_2(void)
{
  return read16u();
}

CAMLexport int caml_deserialize_sint_2(void)
{
  return read16s();
}

CAMLexport uint32 caml_deserialize_uint_4(void)
{
  return read32u();
}

CAMLexport int32 caml_deserialize_sint_4(void)
{
  return read32s();
}

CAMLexport uint64 caml_deserialize_uint_8(void)
{
  uint64 i;
  caml_deserialize_block_8(&i, 1);
  return i;
}

CAMLexport int64 caml_deserialize_sint_8(void)
{
  int64 i;
  caml_deserialize_block_8(&i, 1);
  return i;
}

CAMLexport float caml_deserialize_float_4(void)
{
  float f;
  caml_deserialize_block_4(&f, 1);
  return f;
}

CAMLexport double caml_deserialize_float_8(void)
{
  double f;
  caml_deserialize_block_float_8(&f, 1);
  return f;
}

CAMLexport void caml_deserialize_block_1(void * data, intnat len)
{
  memmove(data, intern_src, len);
  intern_src += len;
}

CAMLexport void caml_deserialize_block_2(void * data, intnat len)
{
#ifndef ARCH_BIG_ENDIAN
  unsigned char * p, * q;
  for (p = intern_src, q = data; len > 0; len--, p += 2, q += 2)
    Reverse_16(q, p);
  intern_src = p;
#else
  memmove(data, intern_src, len * 2);
  intern_src += len * 2;
#endif
}

CAMLexport void caml_deserialize_block_4(void * data, intnat len)
{
#ifndef ARCH_BIG_ENDIAN
  unsigned char * p, * q;
  for (p = intern_src, q = data; len > 0; len--, p += 4, q += 4)
    Reverse_32(q, p);
  intern_src = p;
#else
  memmove(data, intern_src, len * 4);
  intern_src += len * 4;
#endif
}

CAMLexport void caml_deserialize_block_8(void * data, intnat len)
{
#ifndef ARCH_BIG_ENDIAN
  unsigned char * p, * q;
  for (p = intern_src, q = data; len > 0; len--, p += 8, q += 8)
    Reverse_64(q, p);
  intern_src = p;
#else
  memmove(data, intern_src, len * 8);
  intern_src += len * 8;
#endif
}

CAMLexport void caml_deserialize_block_float_8(void * data, intnat len)
{
#if ARCH_FLOAT_ENDIANNESS == 0x01234567
  memmove(data, intern_src, len * 8);
  intern_src += len * 8;
#elif ARCH_FLOAT_ENDIANNESS == 0x76543210
  unsigned char * p, * q;
  for (p = intern_src, q = data; len > 0; len--, p += 8, q += 8)
    Reverse_64(q, p);
  intern_src = p;
#else
  unsigned char * p, * q;
  for (p = intern_src, q = data; len > 0; len--, p += 8, q += 8)
    Permute_64(q, ARCH_FLOAT_ENDIANNESS, p, 0x01234567);
  intern_src = p;
#endif
}

CAMLexport void caml_deserialize_error(char * msg)
{
  intern_cleanup(0); /* !! LEAK */
  caml_failwith(msg);
}
