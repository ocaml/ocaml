#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include "misc.h"
#include "fail.h"
#include "memory.h"
#include "shared_heap.h"
#include "minor_heap.h"
#include "domain.h"
#include "addrmap.h"
#include "roots.h"

static void shared_heap_write_barrier(value obj, int field, value val)
{
  Assert (Is_block(obj) && !Is_young(obj));

  if (Is_block(val)) {
    if (Is_young(val)) {
      /* Add to remembered set */
      Ref_table_add(&caml_ref_table, obj, field);
    } else {
      /* 
         FIXME: should have an is_marking check
         don't want to do this all the time
         
         unconditionally mark new value
      */

      caml_darken(val, 0);
    }
  }
}

static void promoted_write(value obj, int field, value val);

CAMLexport void caml_modify_field (value obj, int field, value val)
{
  Assert (Is_block(obj));
  Assert (!Is_foreign(obj));
  Assert (!Is_foreign(val));
  Assert (!Is_block(val) || Wosize_hd (Hd_val (val)) < (1 << 20)); /* !! */

  Assert(field >= 0 && field < Wosize_val(obj));

  if (Is_promoted_hd(Hd_val(obj))) {
    promoted_write(obj, field, val);
  } else {
    if (!Is_young(obj)) shared_heap_write_barrier(obj, field, val);
    Op_val(obj)[field] = val;
  }
}

CAMLexport void caml_initialize_field (value obj, int field, value val)
{
  /* FIXME: there are more efficient implementations of this */
  Op_val(obj)[field] = Val_long(0);
  caml_modify_field(obj, field, val);
}

static int promoted_cas(value obj, int field, value oldval, value newval);

CAMLexport int caml_atomic_cas_field (value obj, int field, value oldval, value newval)
{
  if (Is_promoted_hd(Hd_val(obj))) {
    return promoted_cas(obj, field, oldval, newval);
  } else {
    value* p = &Op_val(obj)[field];
    if (Is_young(obj)) {
      /* non-atomic CAS since only this thread can access the object */
      if (*p == oldval) {
        *p = newval;
        return 1;
      } else {
        return 0;
      }
    } else {
      /* need a real CAS */
      if (__sync_bool_compare_and_swap(p, oldval, newval)) {
        shared_heap_write_barrier(obj, field, newval);
        return 1;
      } else {
        return 0;
      }
    }
  }
}

CAMLexport void caml_set_fields (value obj, value v)
{
  int i;
  Assert (Is_block(obj));
  
  for (i = 0; i < Wosize_val(obj); i++) {
    caml_modify_field(obj, i, v);
  }
}

CAMLexport void caml_blit_fields (value src, int srcoff, value dst, int dstoff, int n)
{
  int i;
  Assert(Is_block(src));
  Assert(Is_block(dst));
  Assert(srcoff + n <= Wosize_val(src));
  Assert(dstoff + n <= Wosize_val(dst));
  Assert(Tag_val(src) != Infix_tag);
  Assert(Tag_val(dst) != Infix_tag);
  
  /* we can't use memcpy/memmove since they may not do atomic word writes.
     for instance, they may copy a byte at a time */
  if (src == dst && srcoff < dstoff) {
    /* copy descending */
    if (Is_young(dst)) {
      /* dst is young, we copy fields directly. This cannot create old->young
         ptrs, nor break incremental GC of the shared heap */
      for (i = n; i > 0; i--) {
        Op_val(dst)[dstoff + i - 1] = Op_val(src)[srcoff + i - 1];
      }
    } else {
      for (i = n; i > 0; i--) {
        caml_modify_field(dst, dstoff + i - 1, Field(src, srcoff + i - 1));
      }
    }
  } else {
    /* copy ascending */
    if (Is_young(dst)) {
      /* see comment above */
      for (i = 0; i < n; i++) {
        Op_val(dst)[dstoff + i] = Field(src, srcoff + i);
      }
    } else {
      for (i = 0; i < n; i++) {
        caml_modify_field(dst, dstoff + i, Field(src, srcoff + i));
      }
    }
  }
}

CAMLexport value caml_alloc_shr (mlsize_t wosize, tag_t tag)
{
  value* v = caml_shared_try_alloc(wosize, tag, 0);
  if (v == NULL) {
    caml_raise_out_of_memory ();
  }
  return Val_hp(v);
}

struct read_fault_req {
  value obj;
  int field;
  value ret;
};

static void handle_read_fault(struct domain* target, void* reqp) {
  struct read_fault_req* req = reqp;
  value v = Op_val(req->obj)[req->field];
  if (Is_minor(v) && caml_owner_of_young_block(v) == target) {
    caml_gc_log("Handling read fault for domain [%02d]", caml_domain_id(target));
    req->ret = caml_promote(target, v);
    Assert (!Is_minor(req->ret));
    /* Update the field so that future requests don't fault. We must
       use a CAS here, since another thread may modify the field and
       we must avoid overwriting its update */
    caml_atomic_cas_field(req->obj, req->field, v, req->ret);
  } else {
    /* Race condition: by the time we handled the fault, the field was
       already modified and no longer points to our heap.  We recurse
       into the read barrier. This always terminates: in the worst
       case, all domains get tied up servicing one fault and then
       there are no more left running to win the race */
    caml_gc_log("Stale read fault for domain [%02d]", caml_domain_id(target));
    req->ret = caml_read_barrier(req->obj, req->field);
  }
}

CAMLexport value caml_read_barrier(value obj, int field)
{
  value v = Op_val(obj)[field];
  if (Is_foreign(v)) {
    struct read_fault_req req = {obj, field, Val_unit};
    if (Is_young(obj)) {
      /* We can trigger a read fault on a young object only if this is
         the young copy of a promoted object. We should fault on the
         promoted version, instead of the young one */
      Assert(Is_promoted_hd(Hd_val(obj)));
      req.obj = caml_addrmap_lookup(&caml_promotion_table, obj);
    }
    caml_gc_log("Read fault to domain [%02d]", caml_domain_id(caml_owner_of_young_block(v)));
    caml_domain_rpc(caml_owner_of_young_block(v), &handle_read_fault, &req);
    Assert(!Is_minor(req.ret));
    caml_gc_log("Read fault returned (%p)", req.ret);
    return req.ret;
  } else {
    return v;
  }
}

struct write_fault_req {
  value obj;
  int field;
  value val;
};

static void handle_write_fault(struct domain* target, void* reqp) {
  struct write_fault_req* req = reqp;
  if (Is_promoted_hd(Hd_val(req->obj)) &&
      caml_owner_of_shared_block(req->obj) == target) {
    caml_gc_log("Handling write fault for domain [%02d]", caml_domain_id(target));
    value local =
      caml_addrmap_lookup(caml_get_sampled_roots(target)->promotion_rev_table,
                          req->obj);
    Op_val(local)[req->field] = req->val;
    shared_heap_write_barrier(req->obj, req->field, req->val);
    Op_val(req->obj)[req->field] = req->val;
  } else {
    caml_gc_log("Stale write fault for domain [%02d]", caml_domain_id(target));
    /* Race condition: this shared block is now owned by someone else */
    caml_modify_field(req->obj, req->field, req->val);
  }
}

static void promoted_write(value obj, int field, value val)
{
  if (Is_young(obj)) {
    value promoted = caml_addrmap_lookup(&caml_promotion_table, obj);
    shared_heap_write_barrier(promoted, field, val);
    Op_val(promoted)[field] = val;
    Op_val(obj)[field] = val;
  } else {
    struct domain* owner = caml_owner_of_shared_block(obj);
    struct write_fault_req req = {obj, field, val};
    caml_gc_log("Write fault to domain [%02d]", caml_domain_id(owner));
    caml_domain_rpc(owner, &handle_write_fault, &req);
  }
}


struct cas_fault_req {
  value obj;
  int field;
  value oldval;
  value newval;
  int success;
};

static int do_promoted_cas(value local, value promoted, int field, value oldval, value newval) {
  value* p = &Op_val(local)[field];
  if (*p == oldval) {
    *p = newval;
    shared_heap_write_barrier(promoted, field, newval);
    Op_val(promoted)[field] = newval;
    return 1;
  } else {
    return 0;
  }
}

static void handle_cas_fault(struct domain* target, void* reqp) {
  struct cas_fault_req* req = reqp;
  if (Is_promoted_hd(Hd_val(req->obj)) &&
      caml_owner_of_shared_block(req->obj) == target) {
    caml_gc_log("Handling CAS fault for domain [%02d]", caml_domain_id(target));
    value local =
      caml_addrmap_lookup(caml_get_sampled_roots(target)->promotion_rev_table,
                          req->obj);
    req->success = do_promoted_cas(local, req->obj, req->field, req->oldval, req->newval);
  } else {
    caml_gc_log("Stale CAS fault for domain [%02d]", caml_domain_id(target));
    req->success = caml_atomic_cas_field(req->obj, req->field, req->oldval, req->newval);
  }
}

static int promoted_cas(value obj, int field, value oldval, value newval)
{
  if (Is_young(obj)) {
    value promoted = caml_addrmap_lookup(&caml_promotion_table, obj);
    return do_promoted_cas(obj, promoted, field, oldval, newval);
  } else {
    struct domain* owner = caml_owner_of_shared_block(obj);
    struct cas_fault_req req = {obj, field, oldval, newval, 0};
    caml_gc_log("CAS fault to domain [%02d]", caml_domain_id(owner));
    caml_domain_rpc(owner, &handle_cas_fault, &req);
    return req.success;
  }
}
