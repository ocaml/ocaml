(* TEST

flags = "-w A"

* setup-ocamlc.byte-build-env
** ocamlc.byte
compile_only = "true"
*** check-ocamlc.byte-output

*)

let () = try () with Invalid_argument "Any" -> ();;

let () = try () with Match_failure ("Any",_,_) -> ();;

let () = try () with Match_failure (_,0,_) -> ();;

type t =
  | Warn of string  [@ocaml.warn_on_literal_pattern]
  | Without_warning of string
  | Warn' of nativeint [@ocaml.warn_on_literal_pattern]
  | Deep of (string * int) list [@ocaml.warn_on_literal_pattern];;

let f = function
| Warn "anything" -> ()
| Warn _ | Warn' _ | Without_warning _ | Deep _ -> ()
;;

let g = function
| Warn' 0n -> ()
| Warn _ | Warn' _ | Without_warning _ | Deep _ -> ()
;;


let h = function
| Without_warning "outside" -> ()
| Warn _ | Warn' _ | Without_warning _ | Deep _ -> ()
;;

let i = function
| Deep (_ :: _ :: _ :: _) -> ()
| Warn _ | Warn' _ | Without_warning _ | Deep _ -> ()
;;

let j = function
| Deep (_ :: _ :: ("deep",_) :: _) -> ()
| Warn _ | Warn' _ | Without_warning _ | Deep _ -> ()
;;
