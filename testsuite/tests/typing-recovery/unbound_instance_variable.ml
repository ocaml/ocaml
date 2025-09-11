(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

class c = object
  val x = 0
  method m: c = {< y=1 >}
end

class e = object
  val x = 0
  method m: c = {< x=1; k=10 >}
end

class f = object
  val x = 10
  method a : f = {<x = 10>}
  method b : f = {<x = 10; y = 12>}
  method c : f = {<k = 15>}
end

let _ =
  let o = object
    val x = 10
      method a = {< x = 10>}
    end
  and _ = object
    val x = 11
    method foo = x + 1
    method b e = {<x = (e # a); y = 22>}
  end
  in (o # a)
