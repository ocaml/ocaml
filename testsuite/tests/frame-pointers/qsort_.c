#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>

typedef struct frame_info
{
  struct frame_info*  prev;     /* base pointer / frame pointer */
  void*               retaddr;  /* instruction pointer / program counter */
} frame_info;

static frame_info* top_frame = NULL;

value with_frame(value callback)
{
  top_frame = __builtin_frame_address(0);
  value ret = caml_callback(callback, Val_unit);
  top_frame = NULL;
  return ret;
}

value check_frames(value unit)
{
  int count = 0;
  if (!top_frame) caml_failwith("only use inside with_frame");
  struct frame_info* fp = __builtin_frame_address(0);
  while ((uintnat)fp > 4096) {
    if (fp == top_frame) return Val_unit;
    if (count > 1000) caml_failwith("too many frames - loop?");
    count++;
    /* return address should be a readable location */
    (void)(*((volatile char*)fp->retaddr));
    fp = fp->prev;
  }
  caml_failwith("top frame not found");
}

value in_callback(value cb)
{
  return caml_callback(cb, Val_unit);
}

value in_callback_stk(
  value v0, value v1, value v2, value v3, value v4,
  value v5, value v6, value v7, value v8, value v9,
  value cb)
{
  if (Int_val(v0) + Int_val(v1) + Int_val(v2) + Int_val(v3) + Int_val(v4) +
      Int_val(v5) + Int_val(v6) + Int_val(v7) + Int_val(v8) + Int_val(v9)
      != 100)
    caml_failwith("bad args");
  return caml_callback(cb, Val_unit);
}

value in_callback_stk_byte(value* v, int argn)
{
  return in_callback_stk(
    v[0], v[1], v[2], v[3], v[4],
    v[5], v[6], v[7], v[8], v[9],
    v[10]);
}

static value* cmp_fn = NULL;

static int cmp_callback(const void* p_a, const void* p_b)
{
  value* const* a = p_a;
  value* const* b = p_b;
  return Long_val(caml_callback2(*cmp_fn, **a, **b));
}

value sort2(value cmp_clos, value a, value b)
{
  CAMLparam3(cmp_clos, a, b);
  value* vs[2] = {&a, &b};
  cmp_fn = &cmp_clos;
  qsort(vs, 2, sizeof(value*), &cmp_callback);
  cmp_fn = NULL;
  CAMLreturn (caml_alloc_2(0, *vs[0], *vs[1]));
}
