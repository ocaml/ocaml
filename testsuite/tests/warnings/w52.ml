(* TEST
   flags = "-w +A"
   * expect
*)

let () = try () with Invalid_argument "Any" -> ();;
[%%expect{|
Line 1, characters 38-43:
1 | let () = try () with Invalid_argument "Any" -> ();;
                                          ^^^^^
Warning 52 [fragile-literal-pattern]: Code should not depend on the actual values of
this constructor's arguments. They are only for information
and may change in future versions. (See manual section 13.5)
|}];;

let () = try () with Match_failure ("Any",_,_) -> ();;
[%%expect{|
Line 1, characters 35-46:
1 | let () = try () with Match_failure ("Any",_,_) -> ();;
                                       ^^^^^^^^^^^
Warning 52 [fragile-literal-pattern]: Code should not depend on the actual values of
this constructor's arguments. They are only for information
and may change in future versions. (See manual section 13.5)
|}];;

let () = try () with Match_failure (_,0,_) -> ();;
[%%expect{|
Line 1, characters 35-42:
1 | let () = try () with Match_failure (_,0,_) -> ();;
                                       ^^^^^^^
Warning 52 [fragile-literal-pattern]: Code should not depend on the actual values of
this constructor's arguments. They are only for information
and may change in future versions. (See manual section 13.5)
|}];;

type t =
  | Warn of string  [@ocaml.warn_on_literal_pattern]
  | Without_warning of string
  | Warn' of nativeint [@ocaml.warn_on_literal_pattern]
  | Deep of (string * int) list [@ocaml.warn_on_literal_pattern];;
[%%expect{|
type t =
    Warn of string
  | Without_warning of string
  | Warn' of nativeint
  | Deep of (string * int) list
|}];;

let f = function
| Warn "anything" -> ()
| Warn _ | Warn' _ | Without_warning _ | Deep _ -> ();;
[%%expect{|
Line 2, characters 7-17:
2 | | Warn "anything" -> ()
           ^^^^^^^^^^
Warning 52 [fragile-literal-pattern]: Code should not depend on the actual values of
this constructor's arguments. They are only for information
and may change in future versions. (See manual section 13.5)
val f : t -> unit = <fun>
|}];;

let g = function
| Warn' 0n -> ()
| Warn _ | Warn' _ | Without_warning _ | Deep _ -> ();;
[%%expect{|
Line 2, characters 8-10:
2 | | Warn' 0n -> ()
            ^^
Warning 52 [fragile-literal-pattern]: Code should not depend on the actual values of
this constructor's arguments. They are only for information
and may change in future versions. (See manual section 13.5)
val g : t -> unit = <fun>
|}];;

let h = function
| Without_warning "outside" -> ()
| Warn _ | Warn' _ | Without_warning _ | Deep _ -> ();;
[%%expect{|
val h : t -> unit = <fun>
|}];;

let i = function
| Deep (_ :: _ :: _ :: _) -> ()
| Warn _ | Warn' _ | Without_warning _ | Deep _ -> ();;
[%%expect{|
val i : t -> unit = <fun>
|}];;

let j = function
| Deep (_ :: _ :: ("deep",_) :: _) -> ()
| Warn _ | Warn' _ | Without_warning _ | Deep _ -> ();;
[%%expect{|
Line 2, characters 7-34:
2 | | Deep (_ :: _ :: ("deep",_) :: _) -> ()
           ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 52 [fragile-literal-pattern]: Code should not depend on the actual values of
this constructor's arguments. They are only for information
and may change in future versions. (See manual section 13.5)
val j : t -> unit = <fun>
|}];;
