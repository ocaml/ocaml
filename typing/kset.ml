open Types

(* konstraint set *)

type t = Types.konstraint ref

let empty () = ref []
let create k = ref k
let add kset k = kset := !kset @ k

let instance kset t =
  let t' = Ctype.repr t in
  match t'.desc with
  | Tkonst (konst,t'') -> 
let debug = try ignore (Sys.getenv "GCAML_DEBUG"); true with _ -> false in
if debug then 
  Format.fprintf Format.err_formatter "kinst=%a@." 
    Printtyp.type_scheme t
;
      add kset konst; t''
  | _ -> t'
