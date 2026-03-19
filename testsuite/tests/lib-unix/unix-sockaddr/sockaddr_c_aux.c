#if defined(__cplusplus)
#error "Expected a C compiler, got a C++ compiler"
#endif

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/socketaddr.h>

/* Test that union sock_addr_union and socklen_param_type still work.
   struct sockaddr_storage and socklen_t are now preferred.
 */

value stubs(value vdest)
{
  CAMLparam1(vdest);
  CAMLlocal1(vaddr);

  union sock_addr_union addr;
  socklen_param_type addr_len;
  caml_unix_get_sockaddr(vdest, &addr, &addr_len);

  vaddr = caml_unix_alloc_sockaddr(&addr, addr_len, -1);
  CAMLreturn(vaddr);
}
