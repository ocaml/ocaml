(* TEST
 compile_only = "true";
 {
   setup-ocamlc.byte-build-env;
   flags = "-dsource -stop-after parsing";
   ocamlc.byte;
   compiler_reference = "${test_source_directory}/test_type_attributes.ml.reference";
   check-ocamlc.byte-output;
 }{
   setup-ocamlc.byte-build-env;
   flags = "-dsource -stop-after parsing";
   ocamlc.byte;
   compiler_reference = "${test_source_directory}/test_type_attributes.ml.reference";
   check-ocamlc.byte-output;
 }
*)
type ground = (GT.string, ground Std.List.ground) t [@@deriving gaaaaaaa ~options:{ gmap }]

type foo = (GT.string, foo Std.List.ground) t [@@deriving bbbbbbbbbbbb ~options:{ qwer }]
and bar = (GT.float, bar Std.List.ground) t [@@deriving ccccccccccccc ~options:{ asdf }]