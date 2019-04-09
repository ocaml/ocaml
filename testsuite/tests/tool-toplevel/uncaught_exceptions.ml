(* TEST
   * expect
*)

(* PR#8594 *)
Printexc.register_printer (fun e ->
  match e with
    | Division_by_zero -> Some "A division by zero is undefined"
    | _ -> None);;
Printexc.register_printer (fun e ->
  match e with
    | Exit -> Some "Catching an exit"
    | _ -> None);;
raise Not_found;;
[%%expect{|
- : unit = ()
- : unit = ()
Exception: Not_found.
|}];;

raise Exit;;
[%%expect{|
Exception: Catching an exit
|}];;

exception Foo of string;;
[%%expect {|
exception Foo of string
|}];;

raise (Foo "bar");;
[%%expect {|
Exception: Foo "bar".
|}];;

raise Division_by_zero;;
[%%expect {|
Exception: A division by zero is undefined
|}];;
