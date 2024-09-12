#include <string.h>
#ifdef _WIN32
#include <windows.h>
#include <process.h>
#else
#include <pthread.h>
#endif
#define CAML_INTERNALS
#include <caml/mlvalues.h>
#include <caml/gc.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <caml/platform.h>
#include <caml/threads.h>

static CAML_THREAD_FUNCTION thread_func(void *fn) {
  caml_c_thread_register();
  caml_acquire_runtime_system();
  caml_callback((value) fn, Val_unit);
  caml_release_runtime_system();
  caml_c_thread_unregister();
  return 0;
}

value spawn_thread(value clos)
{
#ifdef _WIN32
  HANDLE thread = (HANDLE) _beginthreadex(
    NULL, /* security: handle can't be inherited */
    0,    /* stack size */
    &thread_func,
    (void *) clos,
    0,    /* run immediately */
    NULL  /* thread identifier */
    );
  CloseHandle(thread); /* detach */
#else
  pthread_t thr;
  pthread_attr_t attr;

  pthread_attr_init(&attr);
  pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
  pthread_create(&thr, &attr, thread_func, (void *) clos);
#endif
  return Val_unit;
}
