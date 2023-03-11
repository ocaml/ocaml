#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <caml/callback.h>
#include <caml/mlvalues.h>
#define UNW_LOCAL_ONLY
#include <libunwind.h>

value ml_func_with_10_params_native(value x1, value x2, value x3, value x4,
                                    value x5, value x6, value x7, value x8,
                                    value x9, value x10) {
    return Val_unit;
}

void error() {
  printf("<error>\n");
  exit(1);
}

value ml_perform_stack_walk(value unused) {
    unw_context_t ctxt;
    unw_getcontext(&ctxt);

    unw_cursor_t cursor;
    {
        int result = unw_init_local(&cursor, &ctxt);
        if (result != 0) error();
    }


    for (;;) {
        {
            char procname[256];
            unw_word_t ip_offset; // IP - start_of_proc
            int result = unw_get_proc_name(&cursor, procname, sizeof(procname),
                                           &ip_offset);
            if (result != 0) error();
            if (strlen(procname) > 4 &&
                !memcmp(procname, "caml", 4) &&
                'A' <= procname[4] && procname[4] <= 'Z' &&
                strchr(procname+4, '.')) {
              /* mangled OCaml name, unmangle and print */
              const char* mangled = procname + 4;
              const char* mod_end = strchr(mangled, '.');
              const char* id_begin = strchr(mod_end + 1, '_');
              if (!id_begin) id_begin = mangled + strlen(mangled);
              printf("%.*s.%.*s\n",
                     (int) (mod_end - mangled), mangled,
                     (int) (id_begin - (mod_end + 1)), mod_end + 1);
            } else {
              printf("%s\n", procname);
            }
            if (!strcmp(procname, "main")) break;
        }

        {
            int result = unw_step(&cursor);
            if (result == 0) error(); /* didn't make it to main() */
            if (result < 0) error();
        }
    }

    return Val_unit;
}

value ml_do_no_alloc(value unused) {
    return ml_perform_stack_walk(unused);
}
