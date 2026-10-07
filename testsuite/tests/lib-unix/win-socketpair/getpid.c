#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <caml/mlvalues.h>

/* Unix.getpid returns a process handle on Windows, not the process
   id used in the names of the socket files. */
value caml_test_get_current_process_id(value unit)
{
  return Val_long(GetCurrentProcessId());
}
