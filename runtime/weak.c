/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*              Damien Doligez, projet Para, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1997 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

/* Operations on weak arrays and ephemerons (named ephe here)*/

#include <string.h>

#include "caml/alloc.h"
#include "caml/domain.h"
#include "caml/fail.h"
#include "caml/major_gc.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"
#include "caml/shared_heap.h"
#include "caml/weak.h"

value caml_dummy[] =
  {(value)Make_header(0,Abstract_tag, NOT_MARKABLE),
   Val_unit};
value caml_ephe_none = (value)&caml_dummy[1];

#define None_val (Val_int(0))
#define Some_tag 0

struct caml_ephe_info* caml_alloc_ephe_info ()
{
  struct caml_ephe_info* e =
    caml_stat_alloc_noexc (sizeof(struct caml_ephe_info));
  if(e != NULL)
    memset (e, 0, sizeof(struct caml_ephe_info));
  return e;
}

/* [len] is a value that represents a number of words (fields) */
CAMLprim value caml_ephe_create (value len)
{
  mlsize_t size, i;
  value res;
  caml_domain_state* domain_state = Caml_state;

  size = Long_val (len)
       + 1 /* weak_list */
       + 1 /* owning domain */
       + 1 /* the value */;
  if (size < CAML_EPHE_FIRST_KEY || size > Max_wosize) caml_invalid_argument ("Weak.create");
  res = caml_alloc_shr (size, Abstract_tag);

  Ephe_link(res) = domain_state->ephe_info->live;
  domain_state->ephe_info->live = res;
  Ephe_domain(res) = caml_domain_self();
  for (i = CAML_EPHE_DATA_OFFSET; i < size; i++)
    Op_val(res)[i] = caml_ephe_none;
  return res;
}

CAMLprim value caml_weak_create (value len)
{
  return caml_ephe_create(len);
}

/**
   Specificity of the cleaning phase (Phase_clean):

   The dead keys must be removed from the ephemerons and data removed
   when one the keys is dead. Here we call it cleaning the ephemerons.
   A specific phase of the GC is dedicated to this, Phase_clean. This
   phase is just after the mark phase, so the white values are dead
   values. It iterates the function caml_ephe_clean through all the
   ephemerons.

   However the GC is incremental and ocaml code can run on the middle
   of this cleaning phase. In order to respect the semantic of the
   ephemerons concerning dead values, the getter and setter must work
   as if the cleaning of all the ephemerons have been done at once.

   - key getter: Even if a dead key have not yet been replaced by
     caml_ephe_none, getting it should return none.
   - key setter: If we replace a dead key we need to set the data to
     caml_ephe_none and clean the ephemeron.

     This two cases are dealt by a call to do_check_key_clean that
     trigger the cleaning of the ephemerons when the accessed key is
     dead. This test is fast.

     In the case of value getter and value setter, there is no fast
     test because the removing of the data depend of the deadliness of the keys.
     We must always try to clean the ephemerons.

 */

/* If we are in Phase_sweep_ephe we need to check if the key
   that is going to disappear is dead and so should trigger a cleaning
 */
static void do_check_key_clean(struct domain* d, value e, mlsize_t offset)
{
  CAMLassert (offset >= CAML_EPHE_FIRST_KEY);
  CAMLassert (Ephe_domain(e) == d);

  if (caml_gc_phase != Phase_sweep_ephe) return;

  value elt = Op_val(e)[offset];
  if (elt != caml_ephe_none && Is_block (elt) &&
      !Is_minor (elt) && is_unmarked(elt)) {
    Op_val(e)[offset] = caml_ephe_none;
    Op_val(e)[CAML_EPHE_DATA_OFFSET] = caml_ephe_none;
  }
}

void caml_ephe_clean (struct domain* d, value v) {
  value child;
  int release_data = 0;
  mlsize_t size, i;
  header_t hd;
  CAMLassert (Ephe_domain(v) = d);

  if (caml_gc_phase != Phase_sweep_ephe) return;

  hd = Hd_val(v);
  size = Wosize_hd (hd);
  for (i = CAML_EPHE_FIRST_KEY; i < size; i++) {
    child = Op_val(v)[i];
  ephemeron_again:
    if (child != caml_ephe_none && Is_block(child)) {
      if (Tag_val (child) == Forward_tag) {
        value f = Forward_val (child);
        if (Is_block(f)) {
          if (Tag_val(f) == Forward_tag || Tag_val(f) == Lazy_tag ||
              Tag_val(f) == Double_tag) {
            /* Do not short-circuit the pointer */
          } else {
            Op_val(v)[i] = child = f;
            if (Is_block (f) && Is_minor (f))
              add_to_ephe_ref_table(&Caml_state->minor_tables->ephe_ref, v, i);
            goto ephemeron_again;
          }
        }
      }

      // FIXME: Is_young -> Is_minor here is probably not what we want, fix this.
      if (!Is_minor (child) && is_unmarked(child)) {
        release_data = 1;
        Op_val(v)[i] = caml_ephe_none;
      }
    }
  }

  child = Op_val(v)[CAML_EPHE_DATA_OFFSET];
  if (child != caml_ephe_none) {
    if (release_data) {
      Op_val(v)[CAML_EPHE_DATA_OFFSET] = caml_ephe_none;
    } else {
      CAMLassert (!Is_block(child) || !is_unmarked(child));
    }
  }
}

static void clean_field (struct domain* d, value e, mlsize_t offset)
{
  if (offset == CAML_EPHE_DATA_OFFSET)
    caml_ephe_clean(d, e);
  else
    do_check_key_clean(d, e, offset);
}

static void do_set (struct domain* d, value e, mlsize_t offset, value v)
{
  CAMLassert (Ephe_domain(e) == d ||
              !Is_block(v) || !Is_minor(v)); //blit operations

  if (Is_block(v) && Is_minor(v)) {
    value old = Op_val(e)[offset];
    Op_val(e)[offset] = v;
    if (!(Is_block(old) && Is_minor(old)))
      add_to_ephe_ref_table (&d->state->minor_tables->ephe_ref,
                             e, offset);
  } else {
    Op_val(e)[offset] = v;
  }
}

value caml_bias_ephe_list(value e, struct domain* d)
{
  value last = 0;
  while (e != 0) {
    CAMLassert (Tag_val(e) == Abstract_tag);
    Ephe_domain(e) = d;
    last = e;
    e = Ephe_link(e);
  }
  return last;
}

/*****************************************************************************
 * Core functions
 ****************************************************************************/

typedef struct {
  void* f;
  value* argv;
  int success;
} rpc_payload_t;

static value ephe_set_field_domain (value e, value n, value el,
                             struct domain* d, int* rpc_success)
{
  CAMLparam3(e,n,el);
  mlsize_t offset = Long_val (n);

  if (rpc_success && Ephe_domain(e) == 0) {
    *rpc_success = 0;
    CAMLreturn(Val_unit);
  }
  CAMLassert (Ephe_domain(e) == d);

  clean_field(d, e, offset);
  do_set(d, e, offset, el);
  CAMLreturn(Val_unit);
}

static value ephe_get_field_domain (value e, value n, struct domain* d, int* rpc_success)
{
  CAMLparam2(e, n);
  CAMLlocal2 (res, elt);
  mlsize_t offset = Long_val (n);

  if (rpc_success && Ephe_domain(e) == 0) {
    *rpc_success = 0;
    CAMLreturn(Val_unit);
  }
  CAMLassert (Ephe_domain(e) == d);

  clean_field(d, e, offset);
  elt = Op_val(e)[offset];

  if (elt == caml_ephe_none) {
    res = None_val;
  } else {
    if (rpc_success) {
      elt = Op_val(e)[offset];
    }
    caml_darken (0, elt, 0);
    if (rpc_success) {
      res = caml_alloc_shr (1, Some_tag);
      caml_initialize_field(res, 0, elt);
    } else {
      res = caml_alloc_small (1, Some_tag);
      Field (res, 0) = elt;
    }
  }
  CAMLreturn (res);
}

static value ephe_get_field_copy_domain (value e, value n, struct domain* d, int* rpc_success)
{
  CAMLparam2 (e, n);
  CAMLlocal2 (res, elt);
  mlsize_t i, offset = Long_val (n);
  value v; /* Caution: this is NOT a local root. */
  value f;

  if (rpc_success && Ephe_domain(e) == 0) {
    *rpc_success = 0;
    CAMLreturn(Val_unit);
  }
  CAMLassert (Ephe_domain(e) == d);

  clean_field (d, e, offset);
  v = Op_val(e)[offset];
  if (v == caml_ephe_none) CAMLreturn (None_val);

  /** Don't copy custom_block #7279 */
  if (Is_block(v) && //XXX KC: trunk includes Is_in_heap_or_young(v) &&
      Tag_val(v) != Custom_tag) {
    if (rpc_success) {
      elt = caml_alloc_shr (Wosize_val(v), Tag_val(v));
    } else {
      elt = caml_alloc (Wosize_val(v), Tag_val(v));
      /* The GC may erase or move v during this call to caml_alloc. */
    }
    clean_field (d, e, offset);
    if (rpc_success) {
      v = Op_val(e)[offset];
    } else {
      v = Op_val (e)[offset];
    }
    if (v == caml_ephe_none) CAMLreturn (None_val);

    if (Tag_val(v) < No_scan_tag) {
      for (i = 0; i < Wosize_val(v); i++) {
        f = Op_val(v)[i];
        caml_darken (0, f, 0);
        Store_field(elt, i, f);
      }
    } else {
      memmove (Bp_val(elt), Bp_val(v), Bosize_val(v));
    }
  } else {
    if (rpc_success) {
      Op_val(e)[offset] = elt = v;
    } else {
      caml_darken (0, v, 0);
      elt = v;
    }
  }
  if (rpc_success) {
    res = caml_alloc_shr (1, Some_tag);
    caml_initialize_field(res, 0, elt);
  } else {
    res = caml_alloc_small (1, Some_tag);
    Field (res, 0) = elt;
  }
  CAMLreturn(res);
}

static value ephe_check_field_domain (value e, value n, struct domain* d, int* rpc_success)
{
  CAMLparam2(e,n);
  CAMLlocal1(v);
  mlsize_t offset = Long_val(n);

  if (rpc_success && Ephe_domain(e) == 0) {
    *rpc_success = 0;
    CAMLreturn(Val_unit);
  }
  CAMLassert (Ephe_domain(e) == d);

  clean_field (d, e, offset);
  v = Op_val(e)[offset];
  CAMLreturn(Val_bool(v != caml_ephe_none));
}

static value ephe_blit_field_produce_domain (value es, value ofs, value len,
                                             struct domain* d, int* rpc_success)
{
  CAMLparam3(es, ofs, len);
  CAMLlocal1(ar);
  mlsize_t offset_s = Long_val(ofs);
  mlsize_t length = Long_val(len);
  long i;

  if (rpc_success && Ephe_domain(es) == 0) {
    *rpc_success = 0;
    CAMLreturn(Val_unit);
  }
  CAMLassert (Ephe_domain(es) == d);

  for (i = 0; i < length; i++) {
    caml_darken(0, Op_val(es)[offset_s + i], 0);
  }
  ar = caml_alloc_shr (length, 0);
  for (i = 0; i < length; i++) {
    caml_initialize_field (ar, i, Op_val(es)[offset_s + i]);
  }
  CAMLreturn(ar);
}

static value ephe_blit_field_consume_domain (value ed, value ofd, value ar,
                                             struct domain* d, int* rpc_success)
{
  CAMLparam3(ed,ofd,ar);
  mlsize_t offset_d = Long_val(ofd);
  long i;

  if (rpc_success && Ephe_domain(ed) == 0) {
    *rpc_success = 0;
    CAMLreturn(Val_unit);
  }
  CAMLassert (Ephe_domain(ed) == d);

  for (i = 0; i < Wosize_val(ar); i++) {
    Op_val(ed)[offset_d + i] = Op_val(ar)[i];
  }
  CAMLreturn(Val_unit);
}

static void handle_ephe_rpc (struct domain* d, void* arg, interrupt* done)
{
  rpc_payload_t* p = (rpc_payload_t*)arg;
  CAMLassert (p->success);

  if (p->f == (void*)&ephe_set_field_domain) {
    p->argv[0] = ephe_set_field_domain(p->argv[0], p->argv[1], p->argv[2], d, &p->success);
  } else if (p->f == (void*)&ephe_get_field_domain) {
    p->argv[0] = ephe_get_field_domain (p->argv[0], p->argv[1], d, &p->success);
  } else if (p->f == (void*)&ephe_get_field_copy_domain) {
    p->argv[0] = ephe_get_field_copy_domain (p->argv[0], p->argv[1], d, &p->success);
  } else if (p->f == (void*)&ephe_check_field_domain) {
    p->argv[0] = ephe_check_field_domain (p->argv[0], p->argv[1], d, &p->success);
  } else if (p->f == (void*)&ephe_blit_field_produce_domain) {
    p->argv[0] = ephe_blit_field_produce_domain (p->argv[0], p->argv[1], p->argv[2], d, &p->success);
  } else if (p->f == (void*)&ephe_blit_field_consume_domain) {
    p->argv[0] = ephe_blit_field_consume_domain (p->argv[0], p->argv[1], p->argv[2], d, &p->success);
  }

  caml_acknowledge_interrupt(done);
}

/*****************************************************************************
 * Wrapper functions
 ****************************************************************************/

static value ephe_set_field (value e, mlsize_t offset, value el)
{
  CAMLparam2(e,el);
  struct domain* source = caml_domain_self();
  struct domain* target;

  while (1) {
    target = Ephe_domain(e);
    if (source == target) {
      CAMLreturn(ephe_set_field_domain(e, Val_long(offset), el, source, NULL));
    } else if (target == 0) {
      caml_adopt_orphaned_work();
    } else {
      CAMLlocalN(argv,3);
      argv[0] = e;
      argv[1] = Val_long(offset);
      argv[2] = el;
      rpc_payload_t p;
      p.f = (void*)&ephe_set_field_domain;
      p.argv = argv;
      p.success = 1;
      if (caml_domain_rpc(target, &handle_ephe_rpc, &p) &&
          p.success) {
        CAMLreturn(Val_unit);
      }
    }
  }
}

CAMLprim value caml_ephe_set_key (value e, value n, value el)
{
  mlsize_t offset = Long_val (n) + CAML_EPHE_FIRST_KEY;

  if (offset < CAML_EPHE_FIRST_KEY || offset >= Wosize_val (e)){
    caml_invalid_argument ("Weak.set");
  }
  return ephe_set_field (e, offset, el);
}

CAMLprim value caml_ephe_unset_key (value e, value n)
{
  return caml_ephe_set_key (e, n, caml_ephe_none);
}

value caml_ephe_set_key_option (value e, value n, value el)
{
  if (el != None_val && Is_block (el)) {
    return caml_ephe_set_key (e, n, Op_val(el)[0]);
  } else {
    return caml_ephe_unset_key (e, n);
  }
}

CAMLprim value caml_weak_set (value ar, value n, value el)
{
  return caml_ephe_set_key_option(ar,n,el);
}

CAMLprim value caml_ephe_set_data (value e, value el)
{
  return ephe_set_field (e, CAML_EPHE_DATA_OFFSET, el);
}

CAMLprim value caml_ephe_unset_data (value e)
{
  return caml_ephe_set_data(e, caml_ephe_none);
}

static value ephe_get_field (value e, mlsize_t offset)
{
  CAMLparam1(e);
  struct domain* source = caml_domain_self();
  struct domain* target;

  while (1) {
    target = Ephe_domain(e);
    if (source == target) {
      CAMLreturn(ephe_get_field_domain(e, Val_long(offset), source, NULL));
    } else if (target == 0) {
      caml_adopt_orphaned_work();
    } else {
      CAMLlocalN(argv,2);
      argv[0] = e;
      argv[1] = Val_long(offset);
      rpc_payload_t p;
      p.f = (void*)&ephe_get_field_domain;
      p.argv = argv;
      p.success = 1;
      if (caml_domain_rpc(target, &handle_ephe_rpc, &p) &&
          p.success) {
        CAMLreturn(argv[0]);
      }
    }
  }
}

CAMLprim value caml_ephe_get_key (value e, value n)
{
  mlsize_t offset = Long_val (n) + CAML_EPHE_FIRST_KEY;
  if (offset < CAML_EPHE_FIRST_KEY || offset >= Wosize_val (e)){
    caml_invalid_argument ("Weak.get");
  }
  return ephe_get_field (e, offset);
}

CAMLprim value caml_weak_get (value ar, value n)
{
  return caml_ephe_get_key(ar, n);
}

static value ephe_get_field_copy (value e, mlsize_t offset)
{
  CAMLparam1(e);
  struct domain* source = caml_domain_self();
  struct domain* target;

  while (1) {
    target = Ephe_domain(e);
    if (source == target) {
      CAMLreturn(ephe_get_field_copy_domain(e, Val_long(offset), source, NULL));
    } else if (target == 0) {
      caml_adopt_orphaned_work();
    } else {
      CAMLlocalN(argv,2);
      argv[0] = e;
      argv[1] = Val_long(offset);
      rpc_payload_t p;
      p.f = (void*)&ephe_get_field_copy_domain;
      p.argv = argv;
      p.success = 1;
      if (caml_domain_rpc(target, &handle_ephe_rpc, &p) &&
          p.success) {
        CAMLreturn(argv[0]);
      }
    }
  }
}

CAMLprim value caml_ephe_get_key_copy (value e, value n)
{
  mlsize_t offset = Long_val (n) + CAML_EPHE_FIRST_KEY;
  if (offset < CAML_EPHE_FIRST_KEY || offset >= Wosize_val (e)){
    caml_invalid_argument ("Weak.get");
  }
  return ephe_get_field_copy(e, offset);
}

CAMLprim value caml_weak_get_copy (value e, value n){
  return caml_ephe_get_key_copy(e,n);
}

CAMLprim value caml_ephe_get_data (value e)
{
  return ephe_get_field (e, CAML_EPHE_DATA_OFFSET);
}

CAMLprim value caml_ephe_get_data_copy (value e)
{
  return ephe_get_field_copy (e, CAML_EPHE_DATA_OFFSET);
}

static value ephe_check_field (value e, mlsize_t offset)
{
  CAMLparam1(e);
  struct domain* source = caml_domain_self();
  struct domain* target;

  while (1) {
    target = Ephe_domain(e);
    if (source == target) {
      CAMLreturn(ephe_check_field_domain(e, Val_long(offset), source, NULL));
    } else if (target == 0) {
      caml_adopt_orphaned_work();
    } else {
      CAMLlocalN(argv,2);
      argv[0] = e;
      argv[1] = Val_long(offset);
      rpc_payload_t p;
      p.f = (void*)&ephe_check_field_domain;
      p.argv = argv;
      p.success = 1;
      if (caml_domain_rpc(target, &handle_ephe_rpc, &p) &&
          p.success) {
        CAMLreturn(argv[0]);
      }
    }
  }
}

CAMLprim value caml_ephe_check_key (value e, value n)
{
  mlsize_t offset = Long_val (n) + CAML_EPHE_FIRST_KEY;
  if (offset < CAML_EPHE_FIRST_KEY || offset >= Wosize_val (e)){
    caml_invalid_argument ("Weak.check");
  }
  return ephe_check_field (e, offset);
}

CAMLprim value caml_weak_check (value e, value n)
{
  return caml_ephe_check_key(e,n);
}

CAMLprim value caml_ephe_check_data (value e)
{
  return ephe_check_field (e, CAML_EPHE_DATA_OFFSET);
}

static value ephe_blit_field_produce (value es, mlsize_t offset_s,
                                      mlsize_t length)
{
  CAMLparam1(es);
  CAMLlocal1(ar);
  struct domain* source = caml_domain_self();
  struct domain* target;

  while (1) {
    target = Ephe_domain(es);
    if (source == target) {
      CAMLreturn(ephe_blit_field_produce_domain(es, Val_long(offset_s), Val_long(length), source, 0));
    }
    if (target == 0) {
      caml_adopt_orphaned_work();
    } else {
      CAMLlocalN(argv, 3);
      argv[0] = es;
      argv[1] = Val_long(offset_s);
      argv[2] = Val_long(length);
      rpc_payload_t p;
      p.f = (void*)&ephe_blit_field_produce_domain;
      p.argv = argv;
      p.success = 1;
      if (caml_domain_rpc (target, &handle_ephe_rpc, &p) &&
          p.success) {
        CAMLreturn(argv[0]);
      }
    }
  }
}

static value ephe_blit_field_consume (value ed, mlsize_t offset_d, value ar)
{
  CAMLparam2(ed,ar);
  struct domain* source = caml_domain_self();
  struct domain* target;

  while (1) {
    target = Ephe_domain(ed);
    if (source == target) {
      CAMLreturn(ephe_blit_field_consume_domain(ed, Val_long(offset_d), ar, source, 0));
    }
    if (target == 0) {
      caml_adopt_orphaned_work();
    } else {
      CAMLlocalN(argv, 3);
      argv[0] = ed;
      argv[1] = Val_long(offset_d);
      argv[2] = ar;
      rpc_payload_t p;
      p.f = (void*)&ephe_blit_field_consume_domain;
      p.argv = argv;
      p.success = 1;
      if (caml_domain_rpc (target, &handle_ephe_rpc, &p) &&
          p.success) {
        CAMLreturn(Val_unit);
      }
    }
  }
}

static value ephe_blit_field (value es, mlsize_t offset_s,
                              value ed, mlsize_t offset_d, mlsize_t length)
{
  CAMLparam2(es,ed);
  CAMLlocal1(ar);
  struct domain* my_domain = caml_domain_self();
  struct domain *d1, *d2;
  long i;

  if (length == 0) CAMLreturn(Val_unit);

  while (1) {
    d1 = Ephe_domain(es);
    d2 = Ephe_domain(ed);

    if (my_domain == d1) caml_ephe_clean(d1, es);
    if (my_domain == d2) caml_ephe_clean(d2, ed);

    if (my_domain == d1 && my_domain == d2) {
      if (offset_d < offset_s) {
        for (i = 0; i < length; i++) {
          caml_darken(0, Op_val(es)[offset_s + i], 0);
          do_set(my_domain, ed, offset_d + i, Op_val(es)[offset_s + i]);
        }
      } else {
        for (i = length - 1; i >= 0; i--) {
          caml_darken(0, Op_val(es)[offset_s + i], 0);
          do_set(my_domain, ed, offset_d + i, Op_val(es)[offset_s + i]);
        }
      }
      CAMLreturn(Val_unit);
    } else if (d1 == 0 || d2 == 0) {
      caml_adopt_orphaned_work();
    } else {
      ar = ephe_blit_field_produce (es, offset_s, length);
      ephe_blit_field_consume (ed, offset_d, ar);
      CAMLreturn(Val_unit);
    }
  }
}

CAMLprim value caml_ephe_blit_key (value es, value ofs,
                                   value ed, value ofd, value len)
{
  mlsize_t offset_s = Long_val (ofs) + CAML_EPHE_FIRST_KEY;
  mlsize_t offset_d = Long_val (ofd) + CAML_EPHE_FIRST_KEY;
  mlsize_t length = Long_val (len);

  if (offset_s < CAML_EPHE_FIRST_KEY || offset_s + length > Wosize_val (es)){
    caml_invalid_argument ("Weak.blit");
  }
  if (offset_d < CAML_EPHE_FIRST_KEY || offset_d + length > Wosize_val (ed)){
    caml_invalid_argument ("Weak.blit");
  }
  return ephe_blit_field (es, offset_s, ed, offset_d, length);
}

CAMLprim value caml_ephe_blit_data (value es, value ed)
{
  return ephe_blit_field (es, CAML_EPHE_DATA_OFFSET,
                          ed, CAML_EPHE_DATA_OFFSET, 1);
}

CAMLprim value caml_weak_blit (value es, value ofs,
                      value ed, value ofd, value len)
{
  return caml_ephe_blit_key (es, ofs, ed, ofd, len);
}


