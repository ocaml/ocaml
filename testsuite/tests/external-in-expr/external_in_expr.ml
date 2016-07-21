

(** {2 Check that the address is the same from ocaml and from C} *)
external get_foobar_global: unit -> nativeint = "get_foobar_global"

external get_var_global: unit -> (nativeint [@unboxed]) =
  "get_var_global_boxed" "get_var_global" [@@noalloc]

let () =
  Printf.printf "foobar same address: %b\n%!"
    ((external "foobar") =  (get_foobar_global ()))

let () =
  Printf.printf "var_global same address: %b\n%!"
    ((external "var_global") = (get_var_global ()))

(** {2 Check that the computation is the same} *)

external run_one: nativeint -> Obj.t -> Obj.t = "run_one"
(** apply the function with the given address to the given value and
    return its result *)

external foobar: int -> int = "foobar"

let foobar_dyn (x:int) : int =
  Obj.obj (run_one (external "foobar") (Obj.repr x))

let () =
  Printf.printf "foobar same computation: %b\n%!"
    ((foobar 1) = (foobar_dyn 1))

(** {2 Check that the computation is the same with unboxing} *)

(** Here we suppose that there is only one C function [foobar_unboxed]
    provided by the user *)

external run_one_unbox: nativeint -> Obj.t -> Obj.t = "run_one_unbox"
(** apply the function with the given address to the given value (must
    be an int) and return its result (must be an int) *)

external foobar_native: (int [@untagged]) -> (int [@untagged]) =
  "shouldnotbecalled" "foobar_unboxed" [@@noalloc]

let foobar_unboxed (x:int) : int =
  match Sys.backend_type with
  | Sys.Native -> foobar_native x
  | Sys.Bytecode ->
      Obj.obj (run_one_unbox (external "foobar_unboxed") (Obj.repr x))
  | Sys.Other _ -> assert false

let () =
  Printf.printf "foobar unboxed result: %i\n%!"
    (foobar_unboxed 1)
