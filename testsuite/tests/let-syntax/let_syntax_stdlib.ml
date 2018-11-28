(* TEST
   * expect
*)

(* List *)

let list_map =
  List.Syntax.(
    let+ x = [1; 2; 3] in
    x + 1
  );;
[%%expect{|
val list_map : int list = [2; 3; 4]
|}];;

let list_map_and =
  List.Syntax.(
    let+ x = [1; 2; 3]
    and+ y = [7; 8; 9] in
    x + y
  );;
[%%expect{|
val list_map_and : int list = [8; 9; 10; 9; 10; 11; 10; 11; 12]
|}];;

let list_bind =
  List.Syntax.(
    let* x = [1; 2; 3] in
    let* y = [7; 8; 9] in
    return (x + y)
  );;
[%%expect{|
val list_bind : int list = [8; 9; 10; 9; 10; 11; 10; 11; 12]
|}];;

let list_bind_and =
  List.Syntax.(
    let* x = [1; 2; 3]
    and* y = [7; 8; 9] in
    return (x + y)
  );;
[%%expect{|
val list_bind_and : int list = [8; 9; 10; 9; 10; 11; 10; 11; 12]
|}];;

let list_bind_map =
  List.Syntax.(
    let* x = [1; 2; 3] in
    let+ y = [7; 8; 9] in
    x + y
  );;
[%%expect{|
val list_bind_map : int list = [8; 9; 10; 9; 10; 11; 10; 11; 12]
|}];;


(* Option *)

let option_map =
  Option.Syntax.(
    let+ x = Some 4 in
    x + 1
  );;
[%%expect{|
val option_map : int option = Some 5
|}];;

let option_map2 =
  Option.Syntax.(
    let+ x = None in
    x + 1
  );;
[%%expect{|
val option_map2 : int option = None
|}];;

let option_map_and =
  Option.Syntax.(
    let+ x = Some 5
    and+ y = Some 8 in
    x + y
  );;
[%%expect{|
val option_map_and : int option = Some 13
|}];;

let option_map_and2 =
  Option.Syntax.(
    let+ x = None
    and+ y = Some 8 in
    x + y
  );;
[%%expect{|
val option_map_and2 : int option = None
|}];;

let option_map_and3 =
  Option.Syntax.(
    let+ x = Some 5
    and+ y = None in
    x + y
  );;
[%%expect{|
val option_map_and3 : int option = None
|}];;

let option_bind =
  Option.Syntax.(
    let* x = Some 5 in
    let* y = Some 6 in
    return (x + y)
  );;
[%%expect{|
val option_bind : int option = Some 11
|}];;

let option_bind2 =
  Option.Syntax.(
    let* x = None in
    let* y = Some 7 in
    return (x + y)
  );;
[%%expect{|
val option_bind2 : int option = None
|}];;

let option_bind3 =
  Option.Syntax.(
    let* x = Some 3 in
    let* y = None in
    return (x + y)
  );;
[%%expect{|
val option_bind3 : int option = None
|}];;

let option_bind_and =
  Option.Syntax.(
    let* x = Some 2
    and* y = Some 7 in
    return (x + y)
  );;
[%%expect{|
val option_bind_and : int option = Some 9
|}];;

let option_bind_and2 =
  Option.Syntax.(
    let* x = None
    and* y = Some 9 in
    return (x + y)
  );;
[%%expect{|
val option_bind_and2 : int option = None
|}];;

let option_bind_and3 =
  Option.Syntax.(
    let* x = Some 3
    and* y = None in
    return (x + y)
  );;
[%%expect{|
val option_bind_and3 : int option = None
|}];;

let option_bind_map =
  Option.Syntax.(
    let* x = Some 3 in
    let+ y = Some 7 in
    x + y
  );;
[%%expect{|
val option_bind_map : int option = Some 10
|}];;


(* Result *)

let result_map =
  Result.Syntax.(
    let+ x = Ok 4 in
    x + 1
  );;
[%%expect{|
val result_map : (int, 'a) result = Ok 5
|}];;

let result_map2 =
  Result.Syntax.(
    let+ x = Error "error" in
    x + 1
  );;
[%%expect{|
val result_map2 : (int, string) result = Error "error"
|}];;

let result_map_and =
  Result.Syntax.(
    let+ x = Ok 5
    and+ y = Ok 8 in
    x + y
  );;
[%%expect{|
val result_map_and : (int, 'a) result = Ok 13
|}];;

let result_map_and2 =
  Result.Syntax.(
    let+ x = Error "error"
    and+ y = Ok 8 in
    x + y
  );;
[%%expect{|
val result_map_and2 : (int, string) result = Error "error"
|}];;

let result_map_and3 =
  Result.Syntax.(
    let+ x = Ok 5
    and+ y = Error "error" in
    x + y
  );;
[%%expect{|
val result_map_and3 : (int, string) result = Error "error"
|}];;

let result_map_and4 =
  Result.Syntax.(
    let+ x = Error "error1"
    and+ y = Error "error2" in
    x + y
  );;
[%%expect{|
val result_map_and4 : (int, string) result = Error "error1"
|}];;

let result_bind =
  Result.Syntax.(
    let* x = Ok 5 in
    let* y = Ok 6 in
    return (x + y)
  );;
[%%expect{|
val result_bind : (int, 'a) result = Ok 11
|}];;

let result_bind2 =
  Result.Syntax.(
    let* x = Error "error" in
    let* y = Ok 7 in
    return (x + y)
  );;
[%%expect{|
val result_bind2 : (int, string) result = Error "error"
|}];;

let result_bind3 =
  Result.Syntax.(
    let* x = Ok 3 in
    let* y = Error "error" in
    return (x + y)
  );;
[%%expect{|
val result_bind3 : (int, string) result = Error "error"
|}];;

let result_bind4 =
  Result.Syntax.(
    let* x = Error "error1" in
    let* y = Error "error2" in
    return (x + y)
  );;
[%%expect{|
val result_bind4 : (int, string) result = Error "error1"
|}];;

let result_bind_and =
  Result.Syntax.(
    let* x = Ok 2
    and* y = Ok 7 in
    return (x + y)
  );;
[%%expect{|
val result_bind_and : (int, 'a) result = Ok 9
|}];;

let result_bind_and2 =
  Result.Syntax.(
    let* x = Error "error"
    and* y = Ok 9 in
    return (x + y)
  );;
[%%expect{|
val result_bind_and2 : (int, string) result = Error "error"
|}];;

let result_bind_and3 =
  Result.Syntax.(
    let* x = Ok 3
    and* y = Error "error" in
    return (x + y)
  );;
[%%expect{|
val result_bind_and3 : (int, string) result = Error "error"
|}];;

let result_bind_and4 =
  Result.Syntax.(
    let* x = Error "error1"
    and* y = Error "error2" in
    return (x + y)
  );;
[%%expect{|
val result_bind_and4 : (int, string) result = Error "error1"
|}];;

let result_bind_map =
  Result.Syntax.(
    let* x = Ok 3 in
    let+ y = Ok 7 in
    x + y
  );;
[%%expect{|
val result_bind_map : (int, 'a) result = Ok 10
|}];;

(* Seq *)

let seq_list_map =
  let seq =
    Seq.Syntax.(
      let+ x = List.to_seq [1; 2; 3] in
      x + 1
    )
  in
  List.of_seq seq;;[%%expect{|
val seq_list_map : int list = [2; 3; 4]
|}];;

let seq_map_and =
  let seq =
    Seq.Syntax.(
      let+ x = List.to_seq [1; 2; 3]
      and+ y = List.to_seq [7; 8; 9] in
      x + y
    )
  in
  List.of_seq seq;;[%%expect{|
val seq_map_and : int list = [8; 9; 10; 9; 10; 11; 10; 11; 12]
|}];;

let seq_bind =
  let seq =
    Seq.Syntax.(
      let* x = List.to_seq [1; 2; 3] in
      let* y = List.to_seq [7; 8; 9] in
      return (x + y)
    )
  in
  List.of_seq seq;;[%%expect{|
val seq_bind : int list = [8; 9; 10; 9; 10; 11; 10; 11; 12]
|}];;

let seq_bind_and =
  let seq =
    Seq.Syntax.(
      let* x = List.to_seq [1; 2; 3]
      and* y = List.to_seq [7; 8; 9] in
      return (x + y)
    )
  in
  List.of_seq seq;;
[%%expect{|
val seq_bind_and : int list = [8; 9; 10; 9; 10; 11; 10; 11; 12]
|}];;

let seq_bind_map =
  let seq =
    Seq.Syntax.(
      let* x = List.to_seq [1; 2; 3] in
      let+ y = List.to_seq [7; 8; 9] in
      x + y
    )
  in
  List.of_seq seq;;

[%%expect{|
val seq_bind_map : int list = [8; 9; 10; 9; 10; 11; 10; 11; 12]
|}];;
