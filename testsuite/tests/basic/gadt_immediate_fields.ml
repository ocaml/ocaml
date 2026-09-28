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

(* Components of a tuple *)
let[@inline never] tuple (type a) (p : a * a t) =
  match p with
  | (n, Int) -> n = 42
  | (s, Str) -> Gc.minor (); s == !keep

(* Same, with the rows in the reverse order (depending on the kind of
   pattern, the environment of either the first or the last row is used) *)
let[@inline never] tuple_rev (type a) (p : a * a t) =
  match p with
  | (s, Str) -> Gc.minor (); s == !keep
  | (n, Int) -> n = 42

(* Arguments of a constructor whose type mentions a locally abstract type *)
let[@inline never] constructor (type a) (x : a) (w : a t) =
  let module M = struct type u = C of a * a t end in
  match Sys.opaque_identity (M.C (x, w)) with
  | C (n, Int) -> n = 42
  | C (s, Str) -> Gc.minor (); s == !keep

let[@inline never] constructor_rev (type a) (x : a) (w : a t) =
  let module M = struct type u = C of a * a t end in
  match Sys.opaque_identity (M.C (x, w)) with
  | C (s, Str) -> Gc.minor (); s == !keep
  | C (n, Int) -> n = 42

(* Fields of a record whose type mentions a locally abstract type *)
let[@inline never] record (type a) (x : a) (w : a t) =
  let module M = struct type u = { v : a; w : a t } end in
  match Sys.opaque_identity { M.v = x; w } with
  | { w = Int; v = n } -> n = 42
  | { w = Str; v = s } -> Gc.minor (); s == !keep

let () =
  assert (tuple (Sys.opaque_identity (42, Int)));
  assert (tuple (Sys.opaque_identity (fresh (), Str)));
  assert (tuple_rev (Sys.opaque_identity (42, Int)));
  assert (tuple_rev (Sys.opaque_identity (fresh (), Str)));
  assert (constructor 42 Int);
  assert (constructor (fresh ()) Str);
  assert (constructor_rev 42 Int);
  assert (constructor_rev (fresh ()) Str);
  assert (record 42 Int);
  assert (record (fresh ()) Str);
  print_endline "OK"
