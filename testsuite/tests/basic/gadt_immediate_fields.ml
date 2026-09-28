(* TEST *)

(* The pattern-matching compiler must not rely on the GADT equations
   introduced by one row to decide that a field read shared by all the
   rows is an immediate. Otherwise, in native code, the field is not
   registered as a GC root, and is not updated when the minor collection
   below moves the string to the major heap. *)

type _ t = Int : int t | Str : string t

(* Another reference to the string, which is updated by the minor
   collection *)
let keep = ref ""

let fresh () =
  let s = String.init 5 (fun i -> "hello".[i]) in
  keep := s;
  s

(* Fields of a record whose type mentions a locally abstract type *)
let[@inline never] record (type a) (x : a) (w : a t) =
  let module M = struct type u = { v : a; w : a t } end in
  match Sys.opaque_identity { M.v = x; w } with
  | { w = Int; v = n } -> n = 42
  | { w = Str; v = s } -> Gc.minor (); s == !keep

let () =
  assert (record 42 Int);
  assert (record (fresh ()) Str);
  print_endline "OK"
