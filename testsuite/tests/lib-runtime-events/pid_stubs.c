#define CAML_NAME_SPACE

#include <caml/mlvalues.h>

#ifdef _WIN32

#include <windows.h>

/* Runtime_events names a ring file after GetCurrentProcessId, and
   Runtime_events.create_cursor takes that same number. The Unix library does
   not deal in those numbers on Windows: Unix.create_process returns a process
   handle (createprocess.c) and Unix.getpid returns a duplicated handle for the
   current process (startup.c). Both have to be translated. */

/* Must be called before Unix.waitpid, which closes the handle. */
value caml_test_pid_of_process(value v)
{
  return Val_long(GetProcessId((HANDLE) Long_val(v)));
}

value caml_test_current_pid(value unit)
{
  return Val_long(GetCurrentProcessId());
}

#else

#include <unistd.h>

/* Elsewhere create_process returns the pid, and getpid is the pid. */
value caml_test_pid_of_process(value v)
{
  return v;
}

value caml_test_current_pid(value unit)
{
  return Val_long(getpid());
}

#endif
