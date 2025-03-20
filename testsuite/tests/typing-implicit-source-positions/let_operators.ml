(* TEST
   expect;
*)

let ( let+ ) ?(call_pos = [%call_pos]) a f = f (call_pos, a);;
[%%expect{|
val ( let+ ) :
  ?call_pos:[%call_pos] -> 'a -> (lexing_location * 'a -> 'b) -> 'b = <fun>
|}]

(* Would be nice to add support for implicit position parameters and (also maybe optional
   arguments) for let operators. *)
let _ =
  let+ (call_pos, a) = 1 in
  call_pos

[%%expect{|
Line 2, characters 2-6:
2 |   let+ (call_pos, a) = 1 in
      ^^^^
Error: The operator "let+" has type
         "?call_pos:[%call_pos] -> 'a -> (lexing_location * 'a -> 'b) -> 'b"
       but it was expected to have type "'c -> ('d -> 'e) -> 'f"
       The first argument is labeled "?call_pos",
       but an unlabeled argument was expected
|}]

let ( let* ) ?(call_pos = 1) a g = g (call_pos, a);;

let _ =
  let* (call_pos, a) = 1 in
  call_pos

[%%expect{|
val ( let* ) : ?call_pos:int -> 'a -> (int * 'a -> 'b) -> 'b = <fun>
Line 4, characters 2-6:
4 |   let* (call_pos, a) = 1 in
      ^^^^
Error: The operator "let*" has type
         "?call_pos:int -> 'a -> (int * 'a -> 'b) -> 'b"
       but it was expected to have type "'c -> ('d -> 'e) -> 'f"
       The first argument is labeled "?call_pos",
       but an unlabeled argument was expected
|}]

(* Infix operators work! *)
let ( >>| ) ?(call_pos = [%call_pos]) x f = f (call_pos, x)
let _ =
  1 >>| fun (call_pos, a) -> call_pos

[%%expect{|
val ( >>| ) :
  ?call_pos:[%call_pos] -> 'a -> (lexing_location * 'a -> 'b) -> 'b = <fun>
- : lexing_location = <location: "", line 3, bytes 2-37>
|}]
