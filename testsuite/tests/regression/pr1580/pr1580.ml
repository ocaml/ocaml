(* TEST *)

(* This function uses a ref initially holding an immediate (None),
   which is later mutated to hold a pointer (Some ...). Despite
   initially holding an immediate, the register used must be marked
   in the frametable.

   This was previously done by distinguishing Const_pointer (values
   like None from a type that also contains pointers) from Const_int
   (values like 0 from a type that contains no pointers), but is
   now done by preserving typing information about the ref. *)

let no_magic b =
  let r = ref None in
  for i = 1 to 10 do
    let z = if b then !r else None in
    Gc.minor ();
    r := Some (String.make i '.');
    (match z with None -> () | Some s -> print_endline s)
  done


(* This version is the same, except uses Obj.magic 0 instead of None.
   This segfaulted when the Const_pointer / Const_int distinction
   was used for register typing, as Obj.magic 0 is a Const_int *)

let light_magic b =
  let none = (Obj.magic 0 : string option) in
  let r = ref none in
  for i = 1 to 10 do
    let z = if b then !r else none in
    Gc.minor ();
    r := Some (String.make i '.');
    (match z with None -> () | Some s -> print_endline s)
  done


(* This version stores references to heap values inside an `int ref`,
   which is eliminated and the resulting register is not marked in
   the frametable. This is not expected to work, segfaults on all
   versions, and is included here only to document what not to do. *)

let dark_magic b =
  let none = 0 in
  let r = ref 0 in
  for i = 1 to 10 do
    let z : string option = Obj.magic (if b then !r else none) in
    Gc.minor ();
    r := Obj.magic (Some (String.make i '.'));
    (match z with None -> () | Some s -> print_endline s)
  done


let () =
  Sys.opaque_identity no_magic true;
  Sys.opaque_identity light_magic true
