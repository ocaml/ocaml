#include <assert.h>
#include <stdatomic.h>
#include <unistd.h>

#include <caml/mlvalues.h>
#include <caml/tsan.h>

#define MAX_WAITGROUP   8
#define SPIN_WAIT_MS    10

typedef struct {
  unsigned    limit; /* Number of threads participating to the checkpoint */
  atomic_uint count; /* Number of threads that have reach the checkpoint */
} waitgroup;

static waitgroup waitgroups[MAX_WAITGROUP] = { 0 };

static atomic_uint index = 0;

CAMLno_tsan static waitgroup* wg_get(unsigned idx)
{
  assert(idx < MAX_WAITGROUP);

  waitgroup* wg = &waitgroups[idx];
  return wg;
}

CAMLno_tsan value wg_create(value n)
{
  waitgroup* wg = wg_get(index);

  wg->limit = Int_val(n);
  wg->count = 0;
  return Val_int(index++);
}

CAMLno_tsan value wg_finish(value t)
{
  waitgroup* wg = wg_get(Int_val(t));

  wg->count += 1;
  return Val_unit;
}

CAMLno_tsan value wg_wait(value t)
{
  waitgroup* wg = wg_get(Int_val(t));

  while (wg->count != wg->limit) { /* spinwait */ }
  return Val_unit;
}
