#define CAML_NAME_SPACE
#define CAML_INTERNALS 1
#include <caml/alloc.h>
#include <caml/osdeps.h>
#include <caml/memory.h>
#include <caml/platform.h>
#include <caml/unixsupport.h>
#include <sys/mman.h>

CAMLprim value stub_asan_enabled(value u)
{
  return Val_bool(caml_mmap_uses_asan());
}

CAMLprim value stub_pagesize(value u)
{
  return Val_int(caml_plat_pagesize);
}

CAMLprim value stub_test_read(value v_offset, value v_len)
{
  CAMLparam2(v_offset, v_len);
  CAMLlocal1(ret);
  intnat offset = Long_val(v_offset);
  size_t len = Long_val(v_len);
  void* addr = caml_mmap(NULL, len, PROT_READ|PROT_WRITE,
                         MAP_PRIVATE|MAP_ANONYMOUS, -1, 0);
  if (MAP_FAILED == addr)
    caml_uerror("caml_mmap", Nothing);
  /* The compiler runtime may have been compiled with ASAN,
   * but those flags don't get automatically inherited by C stubs,
   * so call a function inside the runtime to perform the read
   */
  ret = caml_alloc_initialized_string(1, (char*)addr + offset);
  if (caml_munmap(addr, len) < 0)
    caml_uerror("caml_munmap", Nothing);
  CAMLreturn(ret);
}
