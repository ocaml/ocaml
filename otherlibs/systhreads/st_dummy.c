/* 'otherlibs/threads' used to contain C support code for the OCaml
   Thread module, which has since been migrated into the OCaml runtime itself.

   To avoid breaking build scripts all over the place, we want to keep
   building a C library archive, even if we don't have C code to
   compile anymore. Windows and OSX don't really support creating
   empty archives (they appear to break this use-case every
   few years), so instead we keep a dummy .c file around so that it
   remains non-empty.
*/

int caml_st_dummy_magic_constant = 42;
