(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

class t = object
  val mutable x = 10
  val y = 10
  method foo : unit =
    let () = x <- 11 in
    y <- 11
end

let _ : int =
  let o = object
    val x = 10
    method foo =
      let () = x <- 11 in
      12
  end
  in
  o # foo

class u = object
  val y = 10
  method foo : unit =
    let () = y <- y ^ "hello" in
    y
end

let x : string = 11
