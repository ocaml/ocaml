(* TEST
 flags = "-I ${ocamlsrcdir}/typing -I ${ocamlsrcdir}/parsing";
 include ocamlcommon;
 expect;
*)

let run s =
  let pe = Parse.expression (Lexing.from_string s) in
  let te = Typecore.type_expression Env.initial pe in
  let ute = Untypeast.untype_expression te in
  Format.printf "%a@." Pprintast.expression ute
;;

[%%expect{|
val run : string -> unit = <fun>
|}];;

run {| match None with Some (Some _) -> () | _ -> () |};;

[%%expect{|
match None with | Some (Some _) -> () | _ -> ()
- : unit = ()
|}];;

run {| let open struct type t = { mutable x : int [@atomic] } end in
       let _ = fun (v : t) -> [%atomic.loc v.x] in () |};;
[%%expect{|
let open struct type t = {
                  mutable x: int [@atomic ]} end in
  let _ = fun (v : t) -> [%ocaml.atomic.loc v.x] in ()
- : unit = ()
|}];;

(***********************************)
(* Untypeast/pprintast maintain the arity of a function. *)

(* 4-ary function *)
run {| fun x y z -> function w -> x y z w |};;

[%%expect{|
fun x y z -> function | w -> x y z w
- : unit = ()
|}];;

(* 3-ary function returning a 1-ary function *)
run {| fun x y z -> (function w -> x y z w) |};;

[%%expect{|
fun x y z -> (function | w -> x y z w)
- : unit = ()
|}];;

(***********************************)
(* Untypeast/pprintast correctly handle value binding type annotations. *)

run {| let foo : 'a. 'a -> 'a = fun x -> x in foo |}

[%%expect{|
let foo : 'a . 'a -> 'a = fun x -> x in foo
- : unit = ()
|}];;

run {| let foo : type a . a -> a = fun x -> x in foo |}

[%%expect{|
let foo : 'a . 'a -> 'a = fun (type a) -> (fun x -> x : a -> a) in foo
- : unit = ()
|}]


let run s =
  let pe = Parse.implementation (Lexing.from_string s) in
  let te,_,_,_,_ = Typemod.type_structure Env.initial pe in
  let ute = Untypeast.untype_structure te in
  Format.printf "%a@." Pprintast.structure ute
;;

[%%expect{|
val run : string -> unit = <fun>
|}];;

(* That test would hang before ocaml/ocaml#14105 *)
run {|type t = (::);; let f (x : t) = match x with (::) -> 4|}

[%%expect{|
type t =
  | (::)
let f (x : t) = match x with | (::) -> 4
- : unit = ()
|}]
