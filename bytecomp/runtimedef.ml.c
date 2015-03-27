let builtin_exceptions = [|
#define Exception(name, id, caml_name) #caml_name ;
#include "exceptions.tbl"
#undef Exception
|]

let builtin_primitives = [|
#define Prim(name) #name ;
#include "primitives.tbl"
#undef Prim
|]
