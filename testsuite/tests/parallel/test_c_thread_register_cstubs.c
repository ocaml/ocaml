#include <string.h>
#include <pthread.h>
#include "caml/mlvalues.h"
#include "caml/gc.h"
#include "caml/memory.h"
#include "caml/callback.h"
#include "threads.h"

void *create_root(value v)
{
  value *root = malloc(sizeof(value));
  *root = v;
  caml_register_generational_global_root(root);
  return (void*)root;
}

value consume_root(void *r)
{
  value *root = (value *)r;
  value v = *root;
  caml_remove_generational_global_root(root);
  free(root);
  return v;
}

void *thread_func(void *root)
{
  caml_c_thread_register();
  caml_acquire_runtime_system();
  caml_callback(consume_root(root), Val_unit);
  caml_release_runtime_system();
  caml_c_thread_unregister();
  return 0;
}

value spawn_thread(value clos)
{
  pthread_t thr;
  pthread_attr_t attr;
  void *root = create_root(clos);
  pthread_attr_init(&attr);
  pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
  pthread_create(&thr, &attr, thread_func, root);
  return Val_unit;
}
