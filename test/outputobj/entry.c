#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>

__declspec(dllexport) void __stdcall start_caml_engine() {
  char * argv[2];
  argv[0] = "--";
  argv[1] = NULL;
  caml_startup(argv);
}
