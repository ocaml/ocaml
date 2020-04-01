(* TEST
   * expect *)

match [] with x -> (fun x -> x);;
[%%expect{|
- : 'a -> 'a = <fun>
|}];;

match [] with x -> (fun x -> x) | _ -> .;;
[%%expect{|
- : 'a -> 'a = <fun>
|}];;
