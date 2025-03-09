#define CAML_INTERNALS
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/misc.h>

int main_os(int argc, char_os **argv)
{
  caml_startup(argv);
  return 0;
}
