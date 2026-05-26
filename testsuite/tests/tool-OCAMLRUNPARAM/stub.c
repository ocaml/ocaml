#include <stdio.h>

#define CAML_INTERNALS
#include <caml/mlvalues.h>
#include <caml/startup_aux.h>
#include <caml/gc_ctrl.h>

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

/* Passing an option without a value sets it to 1, for example
   OCAMLRUNPARAM=b is equivalent to OCAMLRUNPARAM='b=1' */
void test_optionless(void)
{
  struct caml_params params;
  caml_init_startup_params(&params);

  // we check that the rightmost setting takes precedence
  const char *opts = "b";
  caml_parse_startup_params(&params, opts);

  printf("OCAMLRUNPARAM='%s' gives b=%" CAML_PRIuNAT "\n",
         opts, params.backtrace_enabled);
}

/* Weird behavior we may not want to preserve: boolean options
   can receive non-boolean values.*/
void test_nonbool(void)
{
  struct caml_params params;
  caml_init_startup_params(&params);

  // we check that the rightmost setting takes precedence
  const char *opts = "b=3";
  caml_parse_startup_params(&params, opts);

  printf("OCAMLRUNPARAM='%s' gives "
         "b=%" CAML_PRIuNAT "\n",
         opts,
         params.backtrace_enabled);
}

/* Weird behavior we may not want to preserve: k, M, G are understood
   as powers of two, even on parameters that are not typically counted
   in binary units. */
void test_weirdly_binary(void)
{
  struct caml_params params;
  caml_init_startup_params(&params);

  // we check that the rightmost setting takes precedence
  const char *opts = "b=2k,d=3M,m=3G";
  caml_parse_startup_params(&params, opts);

  printf("OCAMLRUNPARAM='%s' gives "
         "b=%" CAML_PRIuNAT ", "
         "d=%" CAML_PRIuNAT ", "
         "m=%" CAML_PRIuNAT "\n",
         opts,
         params.backtrace_enabled,
         params.max_domains,
         params.init_custom_minor_ratio);
}

/* Weird behavior we may not want to preserve: options preceded
   by a space are ignored. */
void test_space_ignored(void)
{
  struct caml_params params;
  caml_init_startup_params(&params);

  // we check that the rightmost setting takes precedence
  const char *opts = "R, t=2K";
  caml_parse_startup_params(&params, opts);

  printf("OCAMLRUNPARAM='%s' gives "
         "R=%" CAML_PRIuNAT ", "
         "t=%" CAML_PRIuNAT "\n",
         opts,
         caml_runtime_hashtbl_randomized,
         params.trace_level);
}

value run_tests(value unit)
{
  test_minor_heap_wsz();
  test_optionless();
  test_nonbool();
  test_weirdly_binary();
  test_space_ignored();

  return Val_unit;
}
