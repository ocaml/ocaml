#include "mlvalues.h"
#include "prims.h"

#define Prim(p) extern value p();
#include "primitives.tbl"
#undef Prim

c_primitive caml_builtin_cprim[] = {
#define Prim(p) p,
#include "primitives.tbl"
#undef Prim
};

char * caml_names_of_builtin_cprim[] = {
#define Prim(p) #p,
#include "primitives.tbl"
#undef Prim
};
