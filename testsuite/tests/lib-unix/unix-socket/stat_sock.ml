(* TEST
 include unix;
 hasunix;
 {
   bytecode;
 }{
   native;
 }
*)

(* #15336: stat and lstat on the file a Unix-domain socket is bound to *)

let kind = function
  | Unix.S_SOCK -> "S_SOCK"
  | S_REG -> "S_REG"
  | S_DIR -> "S_DIR"
  | S_LNK -> "S_LNK"
  | S_CHR -> "S_CHR"
  | S_BLK -> "S_BLK"
  | S_FIFO -> "S_FIFO"

let () =
  let path = Filename.temp_file "ocaml-stat" ".sock" in
  Sys.remove path;
  let sock = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () -> Unix.close sock; Sys.remove path)
    (fun () ->
       Unix.bind sock (Unix.ADDR_UNIX path);
       Printf.printf "lstat: %s\n" (kind (Unix.lstat path).st_kind);
       Printf.printf "stat: %s\n" (kind (Unix.stat path).st_kind);
       Printf.printf "LargeFile.lstat: %s\n"
         (kind (Unix.LargeFile.lstat path).st_kind);
       Printf.printf "LargeFile.stat: %s\n"
         (kind (Unix.LargeFile.stat path).st_kind))
