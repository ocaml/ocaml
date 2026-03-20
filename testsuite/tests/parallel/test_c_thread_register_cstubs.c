#include <string.h>
#include <stdbool.h>
#ifdef _WIN32
#include <windows.h>
#include <process.h>
#else
#include <pthread.h>
#endif
#define CAML_INTERNALS
#include <caml/config.h>
#include <caml/mlvalues.h>
#include <caml/gc.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <caml/platform.h>
#include <caml/threads.h>

typedef struct {
  value *root;
  uintnat first_domain_unique_id, second_domain_unique_id;
  bool specific_domain, register_twice;
} thread_args;

// Note: We never release this, to keep the test code simple.
static void *create_root(value v)
{
  value *root = malloc(sizeof(value));
  *root = v;
  caml_register_generational_global_root(root);
  return (void*)root;
}

value root_value(void *r)
{
  value *root = (value *)r;
  return *root;
}

static CAML_THREAD_FUNCTION thread_func(void *arg)
{
  thread_args *args = (thread_args*)arg;

  if (args->specific_domain) {
    if (!caml_c_thread_register_in_domain(args->first_domain_unique_id)) {
      fprintf(stderr, "Failed to register thread in domain %" CAML_PRIuNAT "\n",
          args->first_domain_unique_id);
      fflush(stderr);
      goto end;
    }
  } else {
    caml_c_thread_register();
  }

  caml_acquire_runtime_system();
  caml_callback(root_value(args->root), Val_unit);
  caml_release_runtime_system();
  caml_c_thread_unregister();

  if (args->register_twice) {
    args->register_twice = false;
    args->first_domain_unique_id = args->second_domain_unique_id;
    return thread_func(args);
  }

end:
  free(args);
  return 0;
}

void spawn_thread_internal(bool specific_domain, uintnat first_domain_unique_id,
  uintnat second_domain_unique_id, bool register_twice, void *root)
{
  thread_args *args = (thread_args*)malloc(sizeof(thread_args));
  args->specific_domain = specific_domain;
  args->register_twice = register_twice;
  args->first_domain_unique_id = first_domain_unique_id;
  args->second_domain_unique_id = second_domain_unique_id;
  args->root = root;
#if _WIN32
  HANDLE thread = (HANDLE) _beginthreadex(
      NULL, /* security: handle can't be inherited */
      0,    /* stack size */
      &thread_func,
      args,
      0,    /* run immediately */
      NULL  /* thread identifier */
      );
  CloseHandle(thread); /* detach */
#else
  pthread_t thr;
  pthread_attr_t attr;
  pthread_attr_init(&attr);
  pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
  pthread_create(&thr, &attr, thread_func, args);
#endif
}

CAMLprim value spawn_thread(value clos)
{
  CAMLparam1(clos);
  spawn_thread_internal(false, 0L, 0L, false, create_root(clos));
  CAMLreturn(Val_unit);
}

CAMLprim value spawn_thread_specific_domain(value domain_unique_id, value clos)
{
  CAMLparam2(domain_unique_id, clos);
  spawn_thread_internal(true, Long_val(domain_unique_id), 0L, false,
    create_root(clos));
  CAMLreturn(Val_unit);
}

CAMLprim value spawn_thread_specific_domain_twice(value first_domain_unique_id,
  value second_domain_unique_id, value clos)
{
  CAMLparam3(first_domain_unique_id, second_domain_unique_id, clos);
  spawn_thread_internal(true, Long_val(first_domain_unique_id),
    Long_val(second_domain_unique_id), true, create_root(clos));
  CAMLreturn(Val_unit);
}
