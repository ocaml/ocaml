#define CAML_NAME_SPACE

#include <caml/mlvalues.h>

#include <sys/resource.h>

CAMLprim value get_rusage_maxrss(CAMLunused value unused)
{
  struct rusage rusage;
  getrusage(RUSAGE_SELF, &rusage);
  return Val_long(rusage.ru_maxrss);
}
