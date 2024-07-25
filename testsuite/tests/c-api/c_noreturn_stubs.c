#include <caml/misc.h>
#include <stdlib.h>

CAMLnoret extern void f(void);
CAMLnoreturn_start extern void g(void) CAMLnoreturn_end;
Noreturn extern void h(void);
extern void i(void) Noreturn;

void f(void) { abort(); }
void g(void) { abort(); }
void h(void) { abort(); }
void i(void) { abort(); }
