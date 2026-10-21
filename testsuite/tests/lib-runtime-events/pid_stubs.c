#define CAML_NAME_SPACE

#include <caml/mlvalues.h>

#ifdef _WIN32
#include <windows.h>
#else
#include <unistd.h>
#endif

/* On Windows Unix.create_process and Unix.getpid return handles, but ring
   files are named by process id. */
value caml_test_get_process_id(value v)
{
#ifdef _WIN32
  return Val_long(GetProcessId((HANDLE) Long_val(v)));
#else
  return v;
#endif
}

value caml_test_get_current_process_id(value unit)
{
#ifdef _WIN32
  return Val_long(GetCurrentProcessId());
#else
  return Val_long(getpid());
#endif
}
