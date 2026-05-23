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
         (params.init_minor_heap_wsz == 200 ? "ok" : "error"));
}

void test_mem_unit_k(void)
{
  struct caml_params params;
  caml_init_startup_params(&params);
  const char *opts = "s=1k";
  caml_parse_startup_params(&params, opts);

  if (params.init_minor_heap_wsz == 1 * 1024) {
    printf("OCAMLRUNPARAM='s=1k' gives s=%lu\n", params.init_minor_heap_wsz);
  } else {
    printf("OCAMLRUNPARAM='s=1k' gives unexpected s\n");
  }
}

void test_mem_unit_M(void)
{
  struct caml_params params;
  caml_init_startup_params(&params);
  const char *opts = "s=1M";
  caml_parse_startup_params(&params, opts);

  if (params.init_minor_heap_wsz == 1 * 1024 * 1024) {
    printf("OCAMLRUNPARAM='s=1M' gives s=%lu\n", params.init_minor_heap_wsz);
  } else {
    printf("OCAMLRUNPARAM='s=1M' gives unexpected s\n");
  }
}

void test_mem_unit_G(void)
{
  struct caml_params params;
  caml_init_startup_params(&params);
  const char *opts = "s=1G";
  caml_parse_startup_params(&params, opts);

  if (params.init_minor_heap_wsz == 1 * 1024 * 1024 * 1024) {
    printf("OCAMLRUNPARAM='s=1G' gives s=%lu\n", params.init_minor_heap_wsz);
  } else {
    printf("OCAMLRUNPARAM='s=1G' gives unexpected s\n");
  }
}

void test_mem_unit_w_4096(void)
{
  struct caml_params params;
  caml_init_startup_params(&params);
  const char *opts = "s=4096w";
  caml_parse_startup_params(&params, opts);
  if (params.init_minor_heap_wsz == 4096) {
    printf("OCAMLRUNPARAM='s=4096w' gives s=%lu\n", params.init_minor_heap_wsz);
  } else {
    printf("OCAMLRUNPARAM='s=4096w' gives unexpected s\n");
  }
}

void test_mem_unit_B_4096(void)
{
  struct caml_params params;
  caml_init_startup_params(&params);
  const char *opts = "s=4096B";
  caml_parse_startup_params(&params, opts);

  if (params.init_minor_heap_wsz == 4096) {
    printf("OCAMLRUNPARAM='s=4096B' gives s=%lu\n", params.init_minor_heap_wsz);
  } else {
    printf("OCAMLRUNPARAM='s=4096B' gives unexpected s\n");
  }
}


void test_mem_unit_kw_2(void)
{
  struct caml_params params;
  caml_init_startup_params(&params);
  const char *opts = "s=2kw";
  caml_parse_startup_params(&params, opts);
  unsigned long expected = 2UL * 1024;

  if (params.init_minor_heap_wsz == expected) {
    printf("OCAMLRUNPARAM='s=2kw' gives s=%lu\n", params.init_minor_heap_wsz);
  } else {
    printf("OCAMLRUNPARAM='s=2kw' gives unexpected s\n");
  }
}

void test_mem_unit_kB_2(void)
{
  struct caml_params params;
  caml_init_startup_params(&params);
  const char *opts = "s=2kB";
  caml_parse_startup_params(&params, opts);
  unsigned long bytes = 2UL * 1024;
  unsigned long expected =
      (bytes / sizeof(value)) > 0 ? (bytes / sizeof(value)) : 1;

  if (params.init_minor_heap_wsz == expected) {
    printf("OCAMLRUNPARAM='s=2kB' gives s=%lu\n", params.init_minor_heap_wsz);
  } else {
    printf("OCAMLRUNPARAM='s=2kB' gives unexpected s\n");
  }
}

void test_mem_unit_Mw_1(void)
{
  struct caml_params params;
  caml_init_startup_params(&params);
  const char *opts = "s=1Mw";
  caml_parse_startup_params(&params, opts);
  unsigned long expected = 1UL * 1024 * 1024;

  if (params.init_minor_heap_wsz == expected) {
    printf("OCAMLRUNPARAM='s=1Mw' gives s=%lu\n", params.init_minor_heap_wsz);
  } else {
    printf("OCAMLRUNPARAM='s=1Mw' gives unexpected s\n");
  }
}

void test_mem_unit_MB_1(void)
{
  struct caml_params params;
  caml_init_startup_params(&params);
  const char *opts = "s=1MB";
  caml_parse_startup_params(&params, opts);
  unsigned long bytes = 1UL * 1024 * 1024;
  unsigned long expected = bytes / sizeof(value);

  if (params.init_minor_heap_wsz == expected) {
    printf("OCAMLRUNPARAM='s=1MB' gives s=%lu\n", params.init_minor_heap_wsz);
  } else {
    printf("OCAMLRUNPARAM='s=1MB' gives unexpected s\n");
  }
}

void test_mem_unit_Gw_1(void)
{
  struct caml_params params;
  caml_init_startup_params(&params);
  const char *opts = "s=1Gw";
  caml_parse_startup_params(&params, opts);
  unsigned long expected = 1UL * 1024 * 1024 * 1024;

  if (params.init_minor_heap_wsz == expected) {
    printf("OCAMLRUNPARAM='s=1Gw' gives s=%lu\n", params.init_minor_heap_wsz);
  } else {
    printf("OCAMLRUNPARAM='s=1Gw' gives unexpected s\n");
  }
}

void test_mem_unit_GB_1(void)
{
  struct caml_params params;
  caml_init_startup_params(&params);
  const char *opts = "s=1GB";
  caml_parse_startup_params(&params, opts);
  unsigned long bytes = 1UL * 1024 * 1024 * 1024;
  unsigned long expected = bytes / sizeof(value);

  if (params.init_minor_heap_wsz == expected) {
    printf("OCAMLRUNPARAM='s=1GB' gives s=%lu\n", params.init_minor_heap_wsz);
  } else {
    printf("OCAMLRUNPARAM='s=1GB' gives unexpected s\n");
  }
}

value run_tests(value unit) {
  test_minor_heap_wsz();
  test_mem_unit_k();
  test_mem_unit_M();
  test_mem_unit_G();

  test_mem_unit_w_4096();
  test_mem_unit_B_4096();
  test_mem_unit_kw_2();
  test_mem_unit_kB_2();
  test_mem_unit_Mw_1();
  test_mem_unit_MB_1();
  test_mem_unit_Gw_1();
  test_mem_unit_GB_1();

  return Val_unit;
}

