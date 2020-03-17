(* TEST
   flags = "-g"
   compare_programs = "false" *)

let[@inline never] id x = Sys.opaque_identity x

class foo = object (self)
  val other = new bar "asdf"
  method go : unit =
    id (other#go 1 2 3)
end
and bar _v = object (self)
  method go _ _ _ : unit =
    id (self#bang)
  method bang : unit =
    raise Exit
end

let () =
  Printexc.record_backtrace true;
  let obj = object (self)
    method meth : unit =
      id ((new foo)#go)
  end in
  match obj#meth with
  | _ -> assert false
  | exception Exit ->
     Printexc.print_backtrace stdout
