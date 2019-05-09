(* TEST
   * expect
*)

class c =
object (o)
  method foo = o
end;;
[%%expect {|
class c : object ('a) method foo : 'a end
|}]

class d =
object (o) inherit c
  method bar = fun () ->
    let o = List.fold_right (fun _ o -> o#foo) [] o in
    let o = match () with () -> o in o
end;;
[%%expect {|
class d : object ('a) method bar : unit -> 'a method foo : 'a end
|}]
