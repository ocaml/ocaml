#ifndef CAML_PARAMS_H
#define CAML_PARAMS_H

#include "mlvalues.h"
#include "exec.h"

/* Called only from startup.c */
void caml_init_startup_params();
CAMLextern value caml_maybe_print_stats(value);

#endif /* CAML_PARAMS_H */
