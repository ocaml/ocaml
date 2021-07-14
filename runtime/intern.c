/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

/* Structured input, compact format */

/* The interface of this file is "caml/intext.h" */

#include <string.h>
#include <stdio.h>
#include "caml/alloc.h"
#include "caml/callback.h"
#include "caml/codefrag.h"
#include "caml/config.h"
#include "caml/custom.h"
#include "caml/fail.h"
#include "caml/gc.h"
#include "caml/intext.h"
#include "caml/io.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"
#include "caml/misc.h"
#include "caml/reverse.h"

struct intern_stack;

struct caml_intern_state {
  unsigned char * intern_src;
  /* Reading pointer in block holding input data. */

  unsigned char * intern_input;
  /* Pointer to beginning of block holding input data,
    if non-NULL this pointer will be freed by the cleanup function. */

  struct intern_stack* stack;
  /* Intern stack */

  struct caml_intern_state* prev;
  /* Pointer to the previous state. This is necessary if callbacks into OCaml
   * (say finalisers) perform interning. */
};

struct caml_intern_state* caml_alloc_intern_state ()
{
  struct caml_intern_state* s =
    caml_stat_alloc_noexc(sizeof(struct caml_intern_state));
  if (s == NULL)
    return NULL;

  s->intern_src = NULL;
  s->intern_input = NULL;
  s->stack = NULL;
  s->prev = NULL;

  return s;
}

void caml_free_intern_state (struct caml_intern_state* s)
{
  /* These assertions may fail if the domain terminates while the demarshaling
     is in progress. Likely that the user has not called
     [caml_deserialize_error] to clean up after a failed demarshaling. */
  CAMLassert (s->intern_input == NULL);
  CAMLassert (s->stack == NULL);
  caml_stat_free(s);
}

static char * intern_resolve_code_pointer(unsigned char digest[16],
                                          asize_t offset);

CAMLnoreturn_start
static void intern_bad_code_pointer(unsigned char digest[16])
CAMLnoreturn_end;

Caml_inline unsigned char read8u(struct caml_intern_state* s)
{ return *s->intern_src++; }

Caml_inline signed char read8s(struct caml_intern_state* s)
{ return *s->intern_src++; }

Caml_inline uint16_t read16u(struct caml_intern_state* s)
{
  uint16_t res = (s->intern_src[0] << 8) + s->intern_src[1];
  s->intern_src += 2;
  return res;
}

Caml_inline int16_t read16s(struct caml_intern_state* s)
{
  int16_t res = (s->intern_src[0] << 8) + s->intern_src[1];
  s->intern_src += 2;
  return res;
}

Caml_inline uint32_t read32u(struct caml_intern_state* s)
{
  uint32_t res =
    ((uint32_t)(s->intern_src[0]) << 24) + (s->intern_src[1] << 16)
    + (s->intern_src[2] << 8) + s->intern_src[3];
  s->intern_src += 4;
  return res;
}

Caml_inline int32_t read32s(struct caml_intern_state* s)
{
  int32_t res =
    ((uint32_t)(s->intern_src[0]) << 24) + (s->intern_src[1] << 16)
    + (s->intern_src[2] << 8) + s->intern_src[3];
  s->intern_src += 4;
  return res;
}

#ifdef ARCH_SIXTYFOUR
static uintnat read64u(struct caml_intern_state* s)
{
  uintnat res =
    ((uintnat) (s->intern_src[0]) << 56)
    + ((uintnat) (s->intern_src[1]) << 48)
    + ((uintnat) (s->intern_src[2]) << 40)
    + ((uintnat) (s->intern_src[3]) << 32)
    + ((uintnat) (s->intern_src[4]) << 24)
    + ((uintnat) (s->intern_src[5]) << 16)
    + ((uintnat) (s->intern_src[6]) << 8)
    + (uintnat) (s->intern_src[7]);
  s->intern_src += 8;
  return res;
}
#endif

Caml_inline void readblock(struct caml_intern_state* s,
                           void * dest, intnat len)
{
  memcpy(dest, s->intern_src, len);
  s->intern_src += len;
}

static void intern_save_state ()
{
  struct caml_intern_state *t;

  if (Caml_state->intern_state->intern_src == NULL)
    return;

  /* There are two reasons why you may end up here.
      1. An exception was raised during demarshaling and
        [caml_deserialize_error] was not called.
      2. A finaliser is demarshalling while the demarshalling is in progress.
        In this case, we push the earlier context down the stack.

     The previous state is restored at [intern_cleanup]. */

  t = caml_alloc_intern_state ();
  if (t == NULL)
    caml_fatal_error ("Cannot allocate memory for intern state");
  t->prev = Caml_state->intern_state;
  Caml_state->intern_state = t;
}

static void intern_init(struct caml_intern_state* s, void * src, void * input)
{
  CAMLassert (s->intern_input == NULL);
  s->intern_src = src;
  s->intern_input = input;
}

struct intern_stack;
static void intern_cleanup(struct caml_intern_state* s);

static void readfloat(struct caml_intern_state* s,
                      double * dest, unsigned int code)
{
  if (sizeof(double) != 8) {
    intern_cleanup(s);
    caml_invalid_argument("input_value: non-standard floats");
  }
  readblock(s, (char *) dest, 8);
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

/* [len] is a number of floats */
static void readfloats(struct caml_intern_state* s,
                       double * dest, mlsize_t len, unsigned int code)
{
  mlsize_t i;
  if (sizeof(double) != 8) {
    intern_cleanup(s);
    caml_invalid_argument("input_value: non-standard floats");
  }
  readblock(s, (char *) dest, len * 8);
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
  s->caml__roots_stack.next = CAML_LOCAL_ROOTS;
  CAML_LOCAL_ROOTS = &s->caml__roots_stack;
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
  if (s->curr_vals != s->first_vals)
    caml_stat_free(s->curr_vals);
}

static void stack_realloc(struct intern_stack* s, value save) {
  CAMLparam1(save);
  int i;
  intern_stack_item* new_vals;
  int new_len = s->len * 2;
  /* reallocate stack */
  caml_gc_log("Reallocating intern stack to size %d", new_len);
  if (new_len >= INTERN_STACK_MAX_SIZE) {
    caml_gc_log ("Stack overflow in un-marshaling value");
    stack_free(s);
    caml_raise_out_of_memory();
  }

  new_vals = caml_stat_alloc(new_len * STACK_NFIELDS * sizeof(value));

  for (i = 0; i < s->sp; i++) {
    STACK_VAL(new_vals, i) = STACK_VAL(s->curr_vals, i);
    STACK_FIELD(new_vals, i) = STACK_FIELD(s->curr_vals, i);
    STACK_OP(new_vals, i) = STACK_OP(s->curr_vals, i);
    STACK_ARG(new_vals, i) = STACK_ARG(s->curr_vals, i);
  }

  for (; i < new_len; i++) {
    STACK_VAL(new_vals, i) = Val_unit;
    STACK_FIELD(new_vals, i) = Val_unit;
    STACK_OP(new_vals, i) = Val_unit;
    STACK_ARG(new_vals, i) = Val_unit;
  }

  if (s->curr_vals != s->first_vals) caml_stat_free(s->curr_vals);

  /* register GC root */
  s->curr_vals = new_vals;
  s->len = new_len;
  s->caml__roots_stack.nitems = new_len * STACK_NFIELDS;
  s->caml__roots_stack.tables[0] = (value*)new_vals;
  CAMLreturn0;
}

static void stack_push(struct intern_stack* s, value v,
                       int field, int op, intnat arg) {
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
  return Int_val(STACK_ARG(s->curr_vals, s->sp - 1));
}

static void stack_advance_field(struct intern_stack* s) {
  intnat field, length;

  Assert(!stack_is_empty(s));

  length = Int_val(STACK_ARG(s->curr_vals, s->sp - 1));
  length--;
  if (length == 0) {
    stack_pop(s);
  } else {
    STACK_ARG(s->curr_vals, s->sp - 1) = Val_int(length);
    field = Int_val(STACK_FIELD(s->curr_vals, s->sp - 1));
    field++;
    STACK_FIELD(s->curr_vals, s->sp - 1) = Val_int(field);
  }
}

static void stack_push_items(struct intern_stack* s, value dest, int n) {
  if (n > 0) {
    stack_push(s, dest, 0, OReadItems, n);
  }
}

static void intern_cleanup(struct caml_intern_state* s)
{
  s->intern_src = NULL;
  if (s->intern_input != NULL) {
    caml_stat_free (s->intern_input);
    s->intern_input = NULL;
  }

  if (s->stack)
    stack_free(s->stack);
  s->stack = NULL;

  if (s->prev != NULL) {
    CAMLassert (Caml_state->intern_state == s);
    Caml_state->intern_state = s->prev;
  }
}

static value intern_rec(struct caml_intern_state* s,
                        mlsize_t whsize, mlsize_t num_objects)
{
  int first = 1;
  int curr_field;
  unsigned int code;
  tag_t tag;
  mlsize_t size, len, i;
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
  s->stack = &S;

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
      if (Long_val(Field(dest, 1)) >= 0)
        caml_set_oo_id(dest);
      /* Pop item and iterate */
      stack_pop(&S);
      break;
    case OShift:
      Store_field (dest, curr_field,
                   Field(dest, curr_field) + stack_curr_arg(&S));
      stack_pop(&S);
      break;
    case OReadItems:
      /* Pop item */
      stack_advance_field(&S);
      /* Read a value and set v to this value */
      code = read8u(s);
      if (code >= PREFIX_SMALL_INT) {
        if (code >= PREFIX_SMALL_BLOCK) {
          /* Small block */
          tag = code & 0xF;
          size = (code >> 4) & 0x7;
        read_block:
          if (size == 0) {
            v = Atom(tag);
          } else {
            v = caml_alloc(size, tag);
            for (i = 0; i < size; i++) Field(v, i) = Val_unit;
            if (use_intern_table) {
              CAMLassert (obj_counter < num_objects);
              Store_field(intern_obj_table, obj_counter++, v);
            }
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
          if (use_intern_table) Store_field(intern_obj_table, obj_counter++, v);
          readblock(s, (char *)String_val(v), len);
        } else {
          switch(code) {
          case CODE_INT8:
            v = Val_long(read8s(s));
            break;
          case CODE_INT16:
            v = Val_long(read16s(s));
            break;
          case CODE_INT32:
            v = Val_long(read32s(s));
            break;
          case CODE_INT64:
#ifdef ARCH_SIXTYFOUR
            v = Val_long((intnat) (read64u(s)));
            break;
#else
            intern_cleanup(s);
            caml_failwith("input_value: integer too large");
            break;
#endif
          case CODE_SHARED8:
            ofs = read8u(s);
          read_shared:
            Assert (ofs > 0);
            Assert (ofs <= obj_counter);
            Assert (use_intern_table);
            v = Field(intern_obj_table, obj_counter - ofs);
            break;
          case CODE_SHARED16:
            ofs = read16u(s);
            goto read_shared;
          case CODE_SHARED32:
            ofs = read32u(s);
            goto read_shared;
          case CODE_BLOCK32:
            header = (header_t) read32u(s);
            tag = Tag_hd(header);
            size = Wosize_hd(header);
            goto read_block;
          case CODE_BLOCK64:
#ifdef ARCH_SIXTYFOUR
            header = (header_t) read64u(s);
            tag = Tag_hd(header);
            size = Wosize_hd(header);
            goto read_block;
#else
            intern_cleanup(s);
            caml_failwith("input_value: data block too large");
            break;
#endif
          case CODE_STRING8:
            len = read8u(s);
            goto read_string;
          case CODE_STRING32:
            len = read32u(s);
            goto read_string;
          case CODE_DOUBLE_LITTLE:
          case CODE_DOUBLE_BIG:
            v = caml_alloc(Double_wosize, Double_tag);
            if (use_intern_table) Store_field(intern_obj_table, obj_counter++, v);
            readfloat(s, (double *) v, code);
            break;
          case CODE_DOUBLE_ARRAY8_LITTLE:
          case CODE_DOUBLE_ARRAY8_BIG:
            len = read8u(s);
          read_double_array:
            v = caml_alloc(len * Double_wosize, Double_array_tag);
            if (use_intern_table) Store_field(intern_obj_table, obj_counter++, v);
            readfloats(s, (double *) v, len, code);
            break;
          case CODE_DOUBLE_ARRAY32_LITTLE:
          case CODE_DOUBLE_ARRAY32_BIG:
            len = read32u(s);
            goto read_double_array;
          case CODE_CODEPOINTER:
            ofs = read32u(s);
            readblock(s, digest, 16);
            codeptr = intern_resolve_code_pointer(digest, ofs);
            if (codeptr != NULL) {
              v = (value) codeptr;
            } else {
              const value* function_placeholder =
                caml_named_value ("Debugger.function_placeholder");
              if (function_placeholder) {
                v = *function_placeholder;
              } else {
                intern_cleanup(s);
                intern_bad_code_pointer(digest);
              }
            }
            break;
          case CODE_INFIXPOINTER:
            ofs = read32u(s);
            /* Read a value to dest[curr_field], then offset it by ofs */
            stack_push(&S, dest, curr_field, OShift, ofs);
            stack_push(&S, dest, curr_field, OReadItems, 1);
            continue;  /* with next iteration of main loop, skipping *dest = v */
          case CODE_CUSTOM:
          case CODE_CUSTOM_LEN:
          case CODE_CUSTOM_FIXED: {
            uintnat expected_size, size;
            ops = caml_find_custom_operations((char *) s->intern_src);
            if (ops == NULL) {
              intern_cleanup(s);
              caml_failwith("input_value: unknown custom block identifier");
            }
            if (code != CODE_CUSTOM_LEN && ops->fixed_length == NULL) {
              intern_cleanup(s);
              caml_failwith("input_value: expected a fixed-size custom block");
            }
            while (*s->intern_src++ != 0) /*nothing*/;  /*skip identifier*/
#ifdef ARCH_SIXTYFOUR
            if (code != CODE_CUSTOM_LEN) {
              expected_size = ops->fixed_length->bsize_64;
            } else {
              s->intern_src += 4;
              expected_size = read64u(s);
            }
#else
            if (code != CODE_CUSTOM_LEN) {
              expected_size = ops->fixed_length->bsize_32;
            } else {
              expected_size = read32u(s);
              s->intern_src += 8;
            }
#endif
            v = caml_alloc_custom(ops, expected_size, 0, 0);
            size = ops->deserialize(Data_custom_val(v));
            if (size != expected_size) {
              intern_cleanup(s);
              caml_failwith(
                "input_value: incorrect length of serialized custom block");
            }
            if (use_intern_table) Store_field (intern_obj_table, obj_counter++, v);
            break;
          }
          default:
            intern_cleanup(s);
            caml_failwith("input_value: ill-formed message");
          }
        }
      }
      /* end of case OReadItems */
      if (first) {
        result = v;
        first = 0;
      } else {
        Store_field(dest, curr_field, v);
      }
      break;
    default:
      Assert(0);
    }
  }
  stack_free(&S);
  s->stack = NULL;
  CAMLreturn(result);
}

/* Parsing the header */

struct marshal_header {
  uint32_t magic;
  int header_len;
  uintnat data_len;
  uintnat num_objects;
  uintnat whsize;
};

static void caml_parse_header(struct caml_intern_state* s,
                              char * fun_name,
                              /*out*/ struct marshal_header * h)
{
  char errmsg[100];

  h->magic = read32u(s);
  switch(h->magic) {
  case Intext_magic_number_small:
    h->header_len = 20;
    h->data_len = read32u(s);
    h->num_objects = read32u(s);
#ifdef ARCH_SIXTYFOUR
    read32u(s);
    h->whsize = read32u(s);
#else
    h->whsize = read32u(s);
    read32u(s);
#endif
    break;
  case Intext_magic_number_big:
#ifdef ARCH_SIXTYFOUR
    h->header_len = 32;
    read32u(s);
    h->data_len = read64u(s);
    h->num_objects = read64u(s);
    h->whsize = read64u(s);
#else
    errmsg[sizeof(errmsg) - 1] = 0;
    snprintf(errmsg, sizeof(errmsg) - 1,
             "%s: object too large to be read back on a 32-bit platform",
             fun_name);
    caml_failwith(errmsg);
#endif
    break;
  default:
    errmsg[sizeof(errmsg) - 1] = 0;
    snprintf(errmsg, sizeof(errmsg) - 1,
             "%s: bad object",
             fun_name);
    caml_failwith(errmsg);
  }
}

/* Reading from a channel */

static value input_val_core(struct caml_intern_state* s, struct channel *chan)
{
  intnat r;
  char header[32];
  struct marshal_header h;
  char * block;
  value res;

  if (! caml_channel_binary_mode(chan))
    caml_failwith("input_value: not a binary channel");
  /* Read and parse the header */
  r = caml_really_getblock(chan, header, 20);
  if (r == 0)
    caml_raise_end_of_file();
  else if (r < 20)
    caml_failwith("input_value: truncated object");
  s->intern_src = (unsigned char *) header;
  if (read32u(s) == Intext_magic_number_big) {
    /* Finish reading the header */
    if (caml_really_getblock(chan, header + 20, 32 - 20) < 32 - 20)
      caml_failwith("input_value: truncated object");
  }
  s->intern_src = (unsigned char *) header;
  caml_parse_header(s, "input_value", &h);
  /* Read block from channel */
  block = caml_stat_alloc(h.data_len);
  /* During [caml_really_getblock], concurrent [caml_input_val] operations
     can take place (via signal handlers or context switching in systhreads),
     and [s->intern_input] may change.  So, wait until [caml_really_getblock]
     is over before using [s->intern_input] and the other global vars. */
  if (caml_really_getblock(chan, block, h.data_len) < h.data_len) {
    caml_stat_free(block);
    caml_failwith("input_value: truncated object");
  }
  /* Initialize global state */
  intern_init(s, block, block);
  /* Fill it in */
  res = intern_rec(s, h.whsize, h.num_objects);
  /* Free everything */
  intern_cleanup(s);
  return caml_check_urgent_gc(res);
}

value caml_input_val(struct channel* chan)
{
  intern_save_state ();
  return input_val_core(Caml_state->intern_state, chan);
}

CAMLprim value caml_input_value(value vchan)
{
  CAMLparam1 (vchan);
  struct channel * chan = Channel(vchan);
  CAMLlocal1 (res);

  With_mutex(&chan->mutex, {
    res = caml_input_val(chan);
  } );
  CAMLreturn (res);
}

/* Reading from memory-resident blocks */

CAMLprim value caml_input_value_to_outside_heap(value vchan)
{
  /* XXX KC: outside_heap is ignored */
  return caml_input_value(vchan);
}

static value input_val_from_block(struct caml_intern_state* s,
                                  struct marshal_header * h)
{
  value obj;
  /* Fill it in */
  obj = intern_rec(s, h->whsize, h->num_objects);
  /* Free internal data structures */
  intern_cleanup(s);
  return caml_check_urgent_gc(obj);
}

CAMLexport value caml_input_value_from_malloc(char * data, intnat ofs)
{
  struct marshal_header h;
  struct caml_intern_state* s;

  intern_save_state ();
  s = Caml_state->intern_state;
  intern_init(s, data + ofs, data);

  caml_parse_header(s, "input_value_from_malloc", &h);

  return input_val_from_block(s, &h);
}

/* [len] is a number of bytes */
CAMLexport value caml_input_value_from_block(const char * data, intnat len)
{
  struct marshal_header h;
  struct caml_intern_state* s;

  /* Initialize global state */
  intern_save_state ();
  s = Caml_state->intern_state;
  intern_init(s, (void*)data, NULL);
  caml_parse_header(s, "input_value_from_block", &h);
  if (h.header_len + h.data_len > len)
    caml_failwith("input_val_from_block: bad length");
  return input_val_from_block(s, &h);
}

CAMLexport value caml_input_val_from_bytes(value str, intnat ofs)
{
  CAMLparam1 (str);
  CAMLlocal1 (obj);
  struct marshal_header h;
  struct caml_intern_state* s;
  char *buffer = NULL;
  mlsize_t length;

  /* Copy the string contents to a non-moving buffer */
  length = caml_string_length(str);
  buffer = caml_stat_alloc_noexc(length - ofs);
  if (buffer == NULL)
    caml_failwith ("input_val_from_bytes: out of memory");
  memcpy (buffer, &Byte_u(str, ofs), length - ofs);

  /* Initialize global state */
  intern_save_state ();
  s = Caml_state->intern_state;
  intern_init(s, buffer, buffer);
  caml_parse_header(s, "input_val_from_string", &h);
  if (ofs + h.header_len + h.data_len > caml_string_length(str))
    caml_failwith("input_val_from_string: bad length");
  /* Fill it in */
  obj = intern_rec(s, h.whsize, h.num_objects);
  /* Free everything, including the buffer */
  intern_cleanup(s);
  CAMLreturn (caml_check_urgent_gc(obj));
}

CAMLprim value caml_input_value_from_bytes(value str, value ofs)
{
  return caml_input_val_from_bytes(str, Long_val(ofs));
}

/* [ofs] is a [value] that represents a number of bytes
   result is a [value] that represents a number of bytes
   To handle both the small and the big format,
   we assume 20 bytes are available at [buff + ofs],
   and we return the data size + the length of the part of the header
   that remains to be read. */

CAMLprim value caml_marshal_data_size(value buff, value ofs)
{
  uint32_t magic;
  int header_len;
  uintnat data_len;
  struct caml_intern_state* s;

  intern_save_state ();
  s = Caml_state->intern_state;
  s->intern_src = &Byte_u(buff, Long_val(ofs));
  magic = read32u(s);
  switch(magic) {
  case Intext_magic_number_small:
    header_len = 20;
    data_len = read32u(s);
    break;
  case Intext_magic_number_big:
#ifdef ARCH_SIXTYFOUR
    header_len = 32;
    read32u(s);
    data_len = read64u(s);
#else
    caml_failwith("Marshal.data_size: "
                  "object too large to be read back on a 32-bit platform");
#endif
    break;
  default:
    caml_failwith("Marshal.data_size: bad object");
  }
  return Val_long((header_len - 20) + data_len);
}

/* Resolution of code pointers */

static char * intern_resolve_code_pointer(unsigned char digest[16],
                                          asize_t offset)
{
  struct code_fragment * cf = caml_find_code_fragment_by_digest(digest);
  if (cf != NULL && cf->code_start + offset < cf->code_end)
    return cf->code_start + offset;
  else
    return NULL;
}

static void intern_bad_code_pointer(unsigned char digest[16])
{
  char msg[256];
  snprintf(msg, sizeof(msg),
               "input_value: unknown code module "
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
  return read8u(Caml_state->intern_state);
}

CAMLexport int caml_deserialize_sint_1(void)
{
  return read8s(Caml_state->intern_state);
}

CAMLexport int caml_deserialize_uint_2(void)
{
  return read16u(Caml_state->intern_state);
}

CAMLexport int caml_deserialize_sint_2(void)
{
  return read16s(Caml_state->intern_state);
}

CAMLexport uint32_t caml_deserialize_uint_4(void)
{
  return read32u(Caml_state->intern_state);
}

CAMLexport int32_t caml_deserialize_sint_4(void)
{
  return read32s(Caml_state->intern_state);
}

CAMLexport uint64_t caml_deserialize_uint_8(void)
{
  uint64_t i;
  caml_deserialize_block_8(&i, 1);
  return i;
}

CAMLexport int64_t caml_deserialize_sint_8(void)
{
  int64_t i;
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
  struct caml_intern_state* s = Caml_state->intern_state;
  memcpy(data, s->intern_src, len);
  s->intern_src += len;
}

CAMLexport void caml_deserialize_block_2(void * data, intnat len)
{
  struct caml_intern_state* s = Caml_state->intern_state;
#ifndef ARCH_BIG_ENDIAN
  unsigned char * p, * q;
  for (p = s->intern_src, q = data; len > 0; len--, p += 2, q += 2)
    Reverse_16(q, p);
  s->intern_src = p;
#else
  memcpy(data, s->intern_src, len * 2);
  s->intern_src += len * 2;
#endif
}

CAMLexport void caml_deserialize_block_4(void * data, intnat len)
{
  struct caml_intern_state* s = Caml_state->intern_state;
#ifndef ARCH_BIG_ENDIAN
  unsigned char * p, * q;
  for (p = s->intern_src, q = data; len > 0; len--, p += 4, q += 4)
    Reverse_32(q, p);
  s->intern_src = p;
#else
  memcpy(data, s->intern_src, len * 4);
  s->intern_src += len * 4;
#endif
}

CAMLexport void caml_deserialize_block_8(void * data, intnat len)
{
  struct caml_intern_state* s = Caml_state->intern_state;
#ifndef ARCH_BIG_ENDIAN
  unsigned char * p, * q;
  for (p = s->intern_src, q = data; len > 0; len--, p += 8, q += 8)
    Reverse_64(q, p);
  s->intern_src = p;
#else
  memcpy(data, s->intern_src, len * 8);
  s->intern_src += len * 8;
#endif
}

CAMLexport void caml_deserialize_block_float_8(void * data, intnat len)
{
  struct caml_intern_state* s = Caml_state->intern_state;
#if ARCH_FLOAT_ENDIANNESS == 0x01234567
  memcpy(data, s->intern_src, len * 8);
  s->intern_src += len * 8;
#elif ARCH_FLOAT_ENDIANNESS == 0x76543210
  unsigned char * p, * q;
  for (p = s->intern_src, q = data; len > 0; len--, p += 8, q += 8)
    Reverse_64(q, p);
  s->intern_src = p;
#else
  unsigned char * p, * q;
  for (p = s->intern_src, q = data; len > 0; len--, p += 8, q += 8)
    Permute_64(q, ARCH_FLOAT_ENDIANNESS, p, 0x01234567);
  s->intern_src = p;
#endif
}

CAMLexport void caml_deserialize_error(char * msg)
{
  intern_cleanup(Caml_state->intern_state);
  caml_failwith(msg);
}
