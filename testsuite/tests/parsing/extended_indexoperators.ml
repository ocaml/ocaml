(* TEST
   * expect
   flags = "-dsource"
*)

let (.?[]) = Hashtbl.find_opt
let (.@[]) = Hashtbl.find
let ( .@[]<- ) = Hashtbl.add
let (.@{}) = Hashtbl.find
let ( .@{}<- ) = Hashtbl.add
let (.@()) = Hashtbl.find
let ( .@()<- ) = Hashtbl.add ;;
[%%expect {|

let (.?[]) = Hashtbl.find_opt;;
val ( .?[] ) : ('a, 'b) Hashtbl.t -> 'a -> 'b option = <fun>

let (.@[]) = Hashtbl.find;;
val ( .@[] ) : ('a, 'b) Hashtbl.t -> 'a -> 'b = <fun>

let (.@[]<-) = Hashtbl.add;;
val ( .@[]<- ) : ('a, 'b) Hashtbl.t -> 'a -> 'b -> unit = <fun>

let (.@{}) = Hashtbl.find;;
val ( .@{} ) : ('a, 'b) Hashtbl.t -> 'a -> 'b = <fun>

let (.@{}<-) = Hashtbl.add;;
val ( .@{}<- ) : ('a, 'b) Hashtbl.t -> 'a -> 'b -> unit = <fun>

let (.@()) = Hashtbl.find;;
val ( .@() ) : ('a, 'b) Hashtbl.t -> 'a -> 'b = <fun>

let (.@()<-) = Hashtbl.add;;
val ( .@()<- ) : ('a, 'b) Hashtbl.t -> 'a -> 'b -> unit = <fun>
|}]

let h: (string,int) Hashtbl.t = Hashtbl.create 17;;
[%%expect {|

let h : (string, int) Hashtbl.t = Hashtbl.create 17;;
val h : (string, int) Hashtbl.t = <abstr>
|}]

let () =
  h .@ ("One") <- 1
; assert (h.@{"One"} = 1)
; Format.printf "%d" h.@{"One"}
; assert (h.?["Two"] = None)
[%%expect {|

let () =
  h.@("One") <- 1;
  assert ((h.@{"One"}) = 1);
  Format.printf "%d" (h.@{"One"});
  assert ((h.?["Two"]) = None);;
|}]


(* from GPR#1392 *)
let ( #? ) x y = (x, y)
let ( .%() ) x y = x.(y)
let x = [| 0 |]
let _ = 1 #? x.(0)
let _ = 1 #? x.%(0);;
[%%expect {|

let (#?) x y = (x, y);;
val ( #? ) : 'a -> 'b -> 'a * 'b = <fun>

let (.%()) x y = x.(y);;
val ( .%() ) : 'a array -> int -> 'a = <fun>

let x = [|0|];;
val x : int array = [|0|]

let _ = 1 #? (x.(0));;
- : int * int = (1, 0)

let _ = 1 #? (x.%(0));;
- : int * int = (1, 0)
|}]


(* from GPR#1467 *)
let _ = x.%(((); (); 0))
let _ = x.%((Format.printf "hello"; 0))
[%%expect {|

let _ = x.%(((); (); 0));;
- : int = 0

let _ = x.%((Format.printf "hello"; 0));;
- : int = 0
|}]
