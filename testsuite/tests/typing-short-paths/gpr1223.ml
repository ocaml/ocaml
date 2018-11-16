(* TEST
   flags = " -short-paths "
   modules = "gpr1223_foo.mli gpr1223_bar.mli"
   * toplevel
*)

let y = Gpr1223_bar.N.O.T;;

let x = Gpr1223_bar.M.T;;
