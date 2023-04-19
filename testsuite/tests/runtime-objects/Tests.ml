(* TEST *)

(* Marshaling (cf. PR#5436) *)

(* Note: this test must *not* be made a toplevel or expect-style test,
   because then the Obj.id counter of the compiler implementation
   (called by the bytecode read-eval-print loop) would be the same as
   the Obj.id counter of the test code below. In particular, any
   change to the compiler implementation to use more objects or
   exceptions would change the numbers below, making the test very
   fragile. *)

let r = ref 0;;
let id o = Oo.id o - !r;;
r := Oo.id (object end);;

assert (id (object end) = 1);;
assert (id (object end) = 2);;
let o = object end in
  let s = Marshal.to_string o [] in
  let o' : < > = Marshal.from_string s 0 in
  let o'' : < > = Marshal.from_string s 0 in
  assert ((id o, id o', id o'') = (3, 4, 5));

let o = object val x = 33 method m = x end in
  let s = Marshal.to_string o [Marshal.Closures] in
  let o' : <m:int> = Marshal.from_string s 0 in
  let o'' : <m:int> = Marshal.from_string s 0 in
  assert ((id o, id o', id o'', o#m, o'#m)
          = (6, 7, 8, 33, 33));;

let o = object val x = 33 val y = 44 method m = x end in
  let s = Marshal.to_string (o,o) [Marshal.Closures] in
  let (o1, o2) : (<m:int> * <m:int>) = Marshal.from_string s 0 in
  let (o3, o4) : (<m:int> * <m:int>) = Marshal.from_string s 0 in
  assert ((id o, id o1, id o2, id o3, id o4, o#m, o1#m)
          = (9, 10, 10, 11, 11, 33, 33));;
