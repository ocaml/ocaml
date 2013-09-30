(* camlp4r *)

(* Copyright 2001 INRIA *)

(* Extensible Functions *)

type t 'a 'b = list (matching 'a 'b)
and matching 'a 'b = { patt : patt; has_when : bool; expr : expr 'a 'b }
and patt =
  [ Eapp of list patt
  | Eacc of list patt
  | Econ of string
  | Estr of string
  | Eint of string
  | Etup of list patt
  | Evar of unit ]
and expr 'a 'b = 'a -> option 'b
;

exception Failure;

value empty = [];

(*** Apply ***)

value rec apply_matchings a =
  fun
  [ [m :: ml] ->
      match m.expr a with
      [ None -> apply_matchings a ml
      | x -> x ]
  | [] -> None ]
;

value apply ef a =
  match apply_matchings a ef with
  [ Some x -> x
  | None -> raise Failure ]
;

(*** Trace ***)

value rec list_iter_sep f s =
  fun
  [ [] -> ()
  | [x] -> f x
  | [x :: l] -> do { f x; s (); list_iter_sep f s l } ]
;

value rec print_patt =
  fun
  [ Eapp pl -> list_iter_sep print_patt2 (fun () -> print_string " ") pl
  | p -> print_patt2 p ]
and print_patt2 =
  fun
  [ Eacc pl -> list_iter_sep print_patt1 (fun () -> print_string ".") pl
  | p -> print_patt1 p ]
and print_patt1 =
  fun
  [ Econ s -> print_string s
  | Estr s -> do { print_string "\""; print_string s; print_string "\"" }
  | Eint s -> print_string s
  | Evar () -> print_string "_"
  | Etup pl ->
      do {
        print_string "(";
        list_iter_sep print_patt (fun () -> print_string ", ") pl;
        print_string ")"
      }
  | Eapp _ | Eacc _ as p ->
      do { print_string "("; print_patt p; print_string ")" } ]
;

value print ef =
  List.iter
    (fun m ->
       do {
         print_patt m.patt;
         if m.has_when then print_string " when ..." else ();
         print_newline ()
       })
    ef
;

(*** Extension ***)

value insert_matching matchings (patt, has_when, expr) =
  let m1 = {patt = patt; has_when = has_when; expr = expr} in
  let rec loop =
    fun
    [ [m :: ml] as gml ->
        if m1.has_when && not m.has_when then [m1 :: gml] else
        if not m1.has_when && m.has_when then [m :: loop ml] else
        (* either both or none have a when clause *)
        if compare m1.patt m.patt = 0 then
          if not m1.has_when then [m1 :: ml] else [m1 :: gml]
        else [m :: loop ml]
    | [] -> [m1] ]
  in
  loop matchings
;

(* available extension function *)

value extend ef matchings_def =
  List.fold_left insert_matching ef matchings_def
;
