#define CAML_INTERNALS
#include <caml/mlvalues.h>
#include <caml/io.h>

CAMLprim value caml_fd_of_channel(value vchan)
{
  return Val_int(Channel(vchan)->fd);
}
