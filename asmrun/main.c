/* Start-up code */

#include <stdio.h>
#include <stdlib.h>
#include "gc.h"
#include "gc_ctrl.h"
#include "misc.h"
#include "mlvalues.h"
#include "sys.h"

header_t first_atoms[256];

static void init_atoms()
{
  int i;
  for(i = 0; i < 256; i++) first_atoms[i] = Make_header(0, i, White);
}

extern value caml_start_program P((void));

int main(argc, argv)
     int argc;
     char * argv[];
{
  int verbose_init = 0, percent_free_init = Percent_free_def;
  long minor_heap_init = Minor_heap_def, heap_chunk_init = Heap_chunk_def;
  char * opt;
  value retcode;

#ifdef DEBUG
  verbose_init = 1;
#endif
  /* Runtime options.  The option letter is the first letter of the
     last word of the ML name of the option (see [lib/gc.mli]). */
  opt = getenv ("CAMLRUNPARAM");
  if (opt != NULL){
    while (*opt != '\0'){
      switch (*opt++){
      case 's': sscanf (opt, "=%ld", &minor_heap_init); break;
      case 'i': sscanf (opt, "=%ld", &heap_chunk_init); break;
      case 'o': sscanf (opt, "=%d", &percent_free_init); break;
      case 'v': sscanf (opt, "=%d", &verbose_init); break;
      }
    }
  }
  init_gc (minor_heap_init, heap_chunk_init, percent_free_init, verbose_init);
  init_atoms();
  sys_init(argv);
  retcode = caml_start_program();
  if (retcode == 0) {
    sys_exit(Val_int(0));
  } else {
    fatal_error_arg("Fatal error: uncaught exception %s.\n",
                    String_val(Field(Field(retcode, 0), 0)));
  }
}

