(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

class a = object(self : 'a)
  val x = 10
  method foo : 'a = {<x = 11; x = 12>}
end

class b = object
  inherit a
  method! foo = {< x = 12; x = 13 >}
end

let f =
  let o =
    object
      inherit a
      method! foo = {< x = 12; x = 13 >}
    end
  in o # foo

module R = struct
  let f =
    let o =
      object
        inherit a
        method! foo = {< x = 12; x = 13 >}
      end
    in o # foo
end

let x = (R.f ) # foo
let t : string = 11
