(* TEST
   * expect
*)

type ('a, 'b) eq = Refl : ('a, 'a) eq

type empty = (int, string) eq

type ('a, 'b) t = Left : 'a -> ('a, 'b) t | Right : 'b -> ('a, 'b) t;;

[%%expect{|
type ('a, 'b) eq = Refl : ('a, 'a) eq
type empty = (int, string) eq
type ('a, 'b) t = Left : 'a -> ('a, 'b) t | Right : 'b -> ('a, 'b) t
|}]

let f1 x =
  match x with
  | (None : empty option) -> ()
;;
[%%expect {|
val f1 : empty option -> unit = <fun>
|}]

let f2 () =
  match None with
  | (None : empty option) -> ()
;;
[%%expect {|
val f2 : unit -> unit = <fun>
|}]

let f3 () =
  let x = None in
  match x with
  | (None : empty option) -> ()
;;
[%%expect {|
val f3 : unit -> unit = <fun>
|}]

let f1' x =
  match x with
  | (None : empty option) -> ()
  | Some _ -> .
;;
[%%expect {|
val f1' : empty option -> unit = <fun>
|}]

let f2' () =
  match None with
  | (None : empty option) -> ()
  | Some _ -> .
;;
[%%expect {|
val f2' : unit -> unit = <fun>
|}]

let f3' () =
  let x = None in
  match x with
  | (None : empty option) -> ()
  | Some _ -> .
;;
[%%expect {|
val f3' : unit -> unit = <fun>
|}]


let (Left () : (unit, empty) t) = Left ();;
[%%expect {|
|}]

let f () =
  let Left () = (Left () : (unit, empty) t) in
  ()
;;
[%%expect {|
val f : unit -> unit = <fun>
|}]

let f () =
  let (Left () : (unit, empty) t) = Left () in
  ()
;;
[%%expect{|
val f : unit -> unit = <fun>
|}]

let f () =
  match (Left () : (unit, empty) t) with
  | Left () -> ()
;;
[%%expect {|
val f : unit -> unit = <fun>
|}]

let f () =
  match (Left () : (unit, empty) t) with
  | Left () -> ()
  | Right _ -> .
;;
[%%expect {|
val f : unit -> unit = <fun>
|}]

let f () =
  match Left () with
  | (Left () : (unit, empty) t) -> ()
;;
[%%expect {|
val f : unit -> unit = <fun>
|}]

let f () =
  match Left () with
  | (Left () : (unit, empty) t) -> ()
  | (Right _ : (unit, empty) t) -> .
;;
[%%expect {|
val f : unit -> unit = <fun>
|}]
