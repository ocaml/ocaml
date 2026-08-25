(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

module Constr = struct
  type t = A | B | C

  let get _ _ = A

  let put f = ignore (f () : t)
end

module Record = struct
  type t = { a : int; b : int; c : int }

  let get _ _ = { a = 0; b = 0; c = 0 }

  let put f = ignore (f () : t)
end

module Bool = struct
  type t = true | false

  let get _ _ = true

  let put f = ignore (f () : t)
end

module List = struct
  type 'a t = [] | (::) of 'a * 'a t

  let get _ _ = []

  let put f = ignore (f () : int t)
end

module Unit = struct
  [@@@warning "-redefining-unit"]
  type t = ()

  let get _ _ = ()

  let put f = ignore (f (() : unit) : t)
end

let () =
  match (fun x -> Constr.get () x) with
  | A | B | C -> ()


let () =
  match (fun x -> Record.get () x) with
  | { a; _ } -> ()


let () =
  match (fun x -> Bool.get () x) with
  | true -> ()


let () =
  match (fun x -> Bool.get () x) with
  | false -> ()


let () =
  match (fun x -> List.get () x) with
  | [] -> ()
  | _ :: _ -> ()


let () =
  match (fun x -> Unit.get () x) with
  | () -> ()


let () = Constr.put A
let () = Record.put { a = 0; b = 0; c = 0 }
let () = Bool.put true
let () = Bool.put false
let () = List.put []
let () = List.put [1; 2; 3]
let () = Unit.put ()
let () = ignore ((Record.get ()).a)
let () = (Record.get ()).a <- 5
let () = ignore { (Record.get ()) with a = 5 }
let foo x = Record.put { x with a = 5 }
