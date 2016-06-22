(* bad *)
let f1 () =
  let exception[@static] E in
  E


(* ok *)
let f2 () =
  let exception[@static] E in
  try raise E
  with E -> ()


(* ok *)
let f3 () =
  let exception[@static] E in
  fun () ->
    try raise E
    with E -> ()


(* bad *)
let f4 () =
  let exception[@static] E in
  try
    fun () -> raise E
  with E -> fun () -> ()


(* ok *)
let f5 i =
  let exception[@static] E in
  let exception[@static] F in
  let exception[@static] G of int * int in
  fun () ->
    try
      if i = 1 then raise E
      else if i = 2 then raise Exit
      else if i = 3 then raise Not_found
      else if i = 4 then raise F
      else if i = 5 then raise (G (40, 2))
      else i
    with E | F | Exit -> 0
       | Not_found -> 1
       | G (x, y) -> x + y


(* ok *)
let f6 x =
  let exception[@static] Ret of bool * int in
  let r = ref 0 in
  try
    for _i = 1 to 10000 do
      r := !r + x;
      if !r > 1000 then raise (Ret (true, !r));

      r := !r + x;
      if !r > 1000 then raise (Ret (false, !r));
    done;
    !r
  with
  | Ret (true, x) -> x
  | Ret (false, x) -> x


(* ok *)
let f7 x =
  let exception[@static] Ret of bool * int in
  let exception[@static] Foo in
  let r = ref 0 in
  try
    for _i = 1 to 10000 do
      r := !r + x;
      if !r > 1000 then raise (Ret (true, !r));

      r := !r + x;
      if !r > 1000 then raise (Ret (false, !r));

      if !r = 200 then raise Foo;
      if !r = 100 then raise Exit;
    done;
    !r
  with
  | Ret (true, x) -> x
  | Ret (false, x) -> -x
  | Foo | Exit -> 42


let f8 x =
  let exception[@static] Foo in (* ok *)
  let exception[@static] Ret of bool * int in (* bad *)
  let r = ref 0 in
  try
    for _i = 1 to 10000 do
      r := !r + x;
      if !r > 1000 then raise (Ret (true, !r));

      r := !r + x;
      if !r > 1000 then raise (Ret (false, !r));

      if !r = 200 then raise Foo;
      if !r = 100 then raise Exit;
    done;
    !r
  with
  | Ret (true, x) -> x
  | Foo | Exit -> 42
