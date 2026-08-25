#include <stdio.h>

#define CAML_INTERNALS
#include <caml/mlvalues.h>
#include <caml/startup_aux.h>

void test_minor_heap_wsz(void)
{
  struct caml_params params;
  caml_init_startup_params(&params);

  // we check that the rightmost setting takes precedence
  const char *opts = "s=100,s=200";
  caml_parse_startup_params(&params, opts);

  printf("minor_heap_wsz: %s\n",
    (params.init_minor_heap_wsz == 200 ? "ok" : "error")
  );
}

value run_tests(value unit)
{
  test_minor_heap_wsz();

  return Val_unit;
}
