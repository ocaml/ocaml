#ifndef _fail_
#define _fail_


#include <setjmp.h>
#include "misc.h"
#include "mlvalues.h"

#define OUT_OF_MEMORY_EXN 0     /* "Out_of_memory" */
#define SYS_ERROR_EXN 1         /* "Sys_error" */
#define FAILURE_EXN 2           /* "Failure" */
#define INVALID_EXN 3           /* "Invalid_argument" */
#define END_OF_FILE_EXN 4       /* "End_of_file" */
#define ZERO_DIVIDE_EXN 5       /* "Division_by_zero" */
#define NOT_FOUND_EXN 6         /* "Not_found" */
#define MATCH_FAILURE_EXN 7     /* "Match_failure" */

struct longjmp_buffer {
  jmp_buf buf;
};

extern struct longjmp_buffer * external_raise;
extern value exn_bucket;

void mlraise P((value bucket)) Noreturn;
void raise_constant P((value tag)) Noreturn;
void raise_with_arg P((value tag, value arg)) Noreturn;
void raise_with_string P((value tag, char * msg)) Noreturn;
void failwith P((char *)) Noreturn;
void invalid_argument P((char *)) Noreturn;
void raise_out_of_memory P((void)) Noreturn;
void raise_sys_error P((value)) Noreturn;
void raise_end_of_file P((void)) Noreturn;
void raise_zero_divide P((void)) Noreturn;
void raise_not_found P((void)) Noreturn;

#endif /* _fail_ */
