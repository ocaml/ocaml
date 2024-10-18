(* TEST *)

module type Print = sig
  type t
  val print : t -> unit
end

let print (module P : Print) (x : P.t) = P.print x

let print_endline (module P : Print) (x : P.t) =
  P.print x;
  print_newline ()

module PList (P : Print) = struct
  type t = P.t list

  let rec aux = function
  | [] -> print_string "]"
  | [x] ->
      print (module P) x;
      print_string "]"
  | hd :: tl ->
      print (module P) hd;
      print_string "; ";
      aux tl


  let print l =
    print_string "[";
    aux l
end

module PInt = struct
  type t = int
  let print = print_int
end

module PString = struct
  type t = string
  let print = print_string
end

module PBool = struct
  type t = bool
  let print b =
    if b
    then print_string "true"
    else print_string "false"
end

let () =
  print_endline (module PList(PInt)) [3; 1; 3];
  print_endline (module PList(PBool)) [true; false];
  print_endline (module PInt) 3;;
