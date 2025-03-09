(* TEST
 expect;
*)

let a = Array.make 8 None;;
let _ = Array.fill a 2 3 (Some 42);;
a;;
[%%expect{|
val a : '_weak1 option array =
  [|None; None; None; None; None; None; None; None|]
- : unit = ()
- : int option array =
[|None; None; Some 42; Some 42; Some 42; None; None; None|]
|}]
let _ = Array.fill a 3 1 (Some 0);;
a;;
[%%expect{|
- : unit = ()
- : int option array =
[|None; None; Some 42; Some 0; Some 42; None; None; None|]
|}]
let _ = Array.fill a 3 6 None;;
a;;
[%%expect{|
Exception: Invalid_argument "Array.fill".
|}]
let _ = Array.fill a (-1) 2 None;;
a;;
[%%expect{|
Exception: Invalid_argument "Array.fill".
|}]
let _ = Gc.compact ();;
let _ = Array.fill a 5 1 (Some (if Random.int 2 < 0 then 1 else 2));;
a;;
[%%expect{|
- : unit = ()
- : unit = ()
- : int option array =
[|None; None; Some 42; Some 0; Some 42; Some 2; None; None|]
|}]
let _ = Array.fill a 5 1 None;;
a;;
[%%expect{|
- : unit = ()
- : int option array =
[|None; None; Some 42; Some 0; Some 42; None; None; None|]
|}]


let a = Array.make 8 0.;;
let _ = Array.fill a 2 3 42.;;
a;;
[%%expect{|
val a : float array = [|0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.|]
- : unit = ()
- : float array = [|0.; 0.; 42.; 42.; 42.; 0.; 0.; 0.|]
|}]

let a = [|(1, 'a'); (2, 'b'); (3, 'c')|];;
let _ = Array.split a;;
[%%expect{|
val a : (int * char) array = [|(1, 'a'); (2, 'b'); (3, 'c')|]
- : int array * char array = ([|1; 2; 3|], [|'a'; 'b'; 'c'|])
|}]

let a = [|1; 2; 3|];;
let b = [|'a'; 'b'; 'c'|];;
let _ = Array.combine a b;;
[%%expect{|
val a : int array = [|1; 2; 3|]
val b : char array = [|'a'; 'b'; 'c'|]
- : (int * char) array = [|(1, 'a'); (2, 'b'); (3, 'c')|]
|}]

let _ : int array * char array = Array.split [||];;
[%%expect{|
- : int array * char array = ([||], [||])
|}]

let _ : (int * char) array = Array.combine [||] [||];;
[%%expect{|
- : (int * char) array = [||]
|}]

let _ = Array.combine [||] [|1|];;
[%%expect{|
Exception: Invalid_argument "Array.combine".
|}]

let a = [|1; 2; 3|];;
let _ = Array.find_opt (function 2 -> true | _ -> false) a;;
[%%expect{|
val a : int array = [|1; 2; 3|]
- : int option = Some 2
|}]

let a = [|1; 2; 3|];;
let _ = Array.find_index (function 2 -> true | _ -> false) a;;
[%%expect{|
val a : int array = [|1; 2; 3|]
- : int option = Some 1
|}]

let a = [|1; 2; 3|];;
let _ = Array.find_index (function 42 -> true | _ -> false) a;;
[%%expect{|
val a : int array = [|1; 2; 3|]
- : int option = None
|}]

let a = [|'a'; 'b'; 'c'|];;
let _ = Array.find_map (function 'b' -> Some 121 | _ -> None) a;;
[%%expect{|
val a : char array = [|'a'; 'b'; 'c'|]
- : int option = Some 121
|}]

let a = [|1; 2; 3|];;
let _ = Array.find_mapi (fun i x -> match (i, x) with
  | (1, 2) -> Some(1, 2)
  | _ -> None) a;;
[%%expect{|
val a : int array = [|1; 2; 3|]
- : (int * int) option = Some (1, 2)
|}]

let a = [|1; 2|];;
let _ = Array.find_opt (function 101 -> true | _ -> false) a;;
[%%expect{|
val a : int array = [|1; 2|]
- : int option = None
|}]

let a = [|1; 2|];;
let _ = Array.find_map (fun _ -> None) a;;
[%%expect{|
val a : int array = [|1; 2|]
- : 'a option = None
|}]

let a = [|1; 2|];;
let _ = Array.find_mapi (fun i x -> match (i, x) with
  | (i, 101) -> Some(i, 101)
  | _ -> None) a;;
[%%expect{|
val a : int array = [|1; 2|]
- : (int * int) option = None
|}]

let a = Array.init 8 succ;;
let _ = Array.fold_left_map (fun a b -> a + b, string_of_int b) 0 a;;
a (* [a] is unchanged *);;
[%%expect {|
val a : int array = [|1; 2; 3; 4; 5; 6; 7; 8|]
- : int * string array = (36, [|"1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"|])
- : int array = [|1; 2; 3; 4; 5; 6; 7; 8|]
|}]

let (_ : (_ * unit array)) =
  Array.fold_left_map (fun _ _ -> assert false) 0 [||];;
[%%expect{|
- : int * unit array = (0, [||])
|}]

let a : int array =
  let a = [| 1 ; 2 ; 3 ; 4 |] in
  Array.map_inplace (fun x -> 2 * x) a;
  a
[%%expect{|
val a : int array = [|2; 4; 6; 8|]
|}]

let a : int array =
  let a = [| 1 ; 2 ; 3 ; 4 |] in
  Array.mapi_inplace (fun i x -> 1 + i + x) a;
  a
[%%expect{|
val a : int array = [|2; 4; 6; 8|]
|}]

let a = Array.make_matrix 2 3 "placeholder";;
a.(0).(0) <- "hello";;
a (* other rows are unchanged *);;
[%%expect{|
val a : string array array =
  [|[|"placeholder"; "placeholder"; "placeholder"|];
    [|"placeholder"; "placeholder"; "placeholder"|]|]
- : unit = ()
- : string array array =
[|[|"hello"; "placeholder"; "placeholder"|];
  [|"placeholder"; "placeholder"; "placeholder"|]|]
|}]

let a = Array.init_matrix 2 3 (fun i j -> 10 * i + j);;
[%%expect{|
val a : int array array = [|[|0; 1; 2|]; [|10; 11; 12|]|]
|}]
