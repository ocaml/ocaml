open Types

(* konstraint set *)

type t = Types.konstraint ref

let empty () = ref []
let create k = ref k
let add kset k = kset := !kset @ k

let instance kset t =
  let t' = Ctype.repr t in
  match t'.desc with
  | Tkonst (konst,t'') -> add kset konst; t''
  | _ -> t'
