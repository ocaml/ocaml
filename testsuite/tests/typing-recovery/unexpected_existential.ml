(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

type any = Any: 'a -> any

let Any x = Any ()

let () =
  let Any x = Any () and () = () in
  ()

let () =
  let rec Any x = Any () in
  ()

let () =
  let[@attribute] Any x = Any () in
  ()

class c (Any x) = object end


class d = object(Any x) end

class e = let Any _x = () in object end

module T = struct
  let Any x = Any ()

  let () =
    let Any x = Any () and () = () in
    ()

  let () =
    let rec Any x = Any () in
    ()

  let () =
    let[@attribute] Any x = Any () in
    ()

  class c (Any x) = object end


  class d = object(Any x) end

  class e = let Any _x = () in object end

end
