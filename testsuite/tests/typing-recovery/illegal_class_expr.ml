(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

class a =
  let _ = new b in
  object end
and b =
  let _ = new a in
  object end

class c =
  let _ = new c in
  object end

class d =
  let x() = new d in
  let _y = x() in
  object end

class e = object end
and f =
  let x() = new e in
  let _y = x() in
  object end

let t : string = 10
