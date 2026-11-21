#include <limits.h>
#include <sys/types.h>
#include <caml/mlvalues.h>

CAMLprim value uid_bits(value unit)
{
  return Val_int(sizeof(uid_t) * CHAR_BIT);
}

CAMLprim value gid_bits(value unit)
{
  return Val_int(sizeof(gid_t) * CHAR_BIT);
}

CAMLprim value pid_bits(value unit)
{
  return Val_int(sizeof(pid_t) * CHAR_BIT);
}

CAMLprim value gid_is_unsigned(value unit)
{
  return Val_bool((gid_t)-1 > (gid_t)0);
}
