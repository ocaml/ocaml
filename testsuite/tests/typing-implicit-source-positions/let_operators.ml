(* TEST
   expect;
*)

let ( let+ ) ?(call_loc = [%call_loc]) a f = f (call_loc, a);;
[%%expect{|
val ( let+ ) : ?call_loc:[%call_loc] -> 'a -> (textloc * 'a -> 'b) -> 'b =
  <fun>
|}]

(* Would be nice to add support for implicit position parameters and (also maybe optional
   arguments) for let operators. *)
let _ =
  let+ (call_loc, a) = 1 in
  call_loc

[%%expect{|
Line 2, characters 2-6:
2 |   let+ (call_loc, a) = 1 in
      ^^^^
Error: The operator "let+" has type
         "?call_loc:[%call_loc] -> 'a -> (textloc * 'a -> 'b) -> 'b"
       but it was expected to have type "'c -> ('d -> 'e) -> 'f"
       The first argument is labeled "?call_loc",
       but an unlabeled argument was expected
|}]

let ( let* ) ?(call_loc = 1) a g = g (call_loc, a);;

let _ =
  let* (call_loc, a) = 1 in
  call_loc

[%%expect{|
val ( let* ) : ?call_loc:int -> 'a -> (int * 'a -> 'b) -> 'b = <fun>
Line 4, characters 2-6:
4 |   let* (call_loc, a) = 1 in
      ^^^^
Error: The operator "let*" has type
         "?call_loc:int -> 'a -> (int * 'a -> 'b) -> 'b"
       but it was expected to have type "'c -> ('d -> 'e) -> 'f"
       The first argument is labeled "?call_loc",
       but an unlabeled argument was expected
|}]

(* Infix operators work! *)
let ( >>| ) ?(call_loc = [%call_loc]) x f = f (call_loc, x)
let _ =
  1 >>| fun (call_loc, a) -> call_loc

[%%expect{|
val ( >>| ) : ?call_loc:[%call_loc] -> 'a -> (textloc * 'a -> 'b) -> 'b =
  <fun>
- : textloc = <location: File "", line 3, characters 2-37>
|}]
