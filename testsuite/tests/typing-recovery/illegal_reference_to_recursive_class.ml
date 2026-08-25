(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)


module rec Foo : sig class type c = object method x : int end end = Foo
and Bar : sig class type c = object inherit Foo.c end end = Bar
and Baz : sig class type c = object inherit Bar.c end end = Baz;;
