#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/memory.h>

#ifdef _WIN32
int wmain(int argc, wchar_t ** argv){
#else
int main(int argc, char ** argv){
#endif

  caml_startup(argv);
  return 0;
}
