(* TEST
   include testing
*)

module GPR1493 = struct
  type t1 = { x : int; y : string; }
  type t2 = { a : int; b : string; c : string list; }

  type t = ..
  type t += C1 of t1 | C2 of t2

  let f (x : t) =
    match x with
    | C1 { x; y } -> ()
    | C2 { a;b;c } -> ()
    | _ -> ()
end
