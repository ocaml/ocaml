#include <string.h>
#ifdef _WIN32
#include <windows.h>
#define THREAD_FUNCTION DWORD WINAPI
#else
#include <pthread.h>
#define THREAD_FUNCTION void *
#endif
#include <caml/mlvalues.h>
#include <caml/gc.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <caml/threads.h>

THREAD_FUNCTION thread_func(void *fn) {
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
  CloseHandle(CreateThread(NULL, 0, &thread_func, (void *) clos, 0, NULL));
#else
  pthread_t thr;
  pthread_attr_t attr;

  pthread_attr_init(&attr);
  pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
  pthread_create(&thr, &attr, thread_func, (void *) clos);
#endif
  return Val_unit;
}
