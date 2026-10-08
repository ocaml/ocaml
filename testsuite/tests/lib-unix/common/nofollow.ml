(* TEST
   include unix;
   hasunix;
   {
     bytecode;
   }{
     native;
   }
*)

let open_nofollow path =
  match Unix.openfile path [Unix.O_RDONLY; Unix.O_NOFOLLOW] 0 with
  | fd -> Unix.close fd; "opened"
  | exception Unix.Unix_error (err, _, _) -> Unix.error_message err

let () =
  let fd = Unix.openfile "nofollow.txt" [Unix.O_WRONLY; Unix.O_CREAT] 0o644 in
  Unix.close fd;
  if Sys.win32 then
    assert (open_nofollow "nofollow.txt"
            = Unix.error_message Unix.EINVAL)
  else begin
    Unix.symlink "nofollow.txt" "nofollow.lnk";
    assert (open_nofollow "nofollow.txt" = "opened");
    assert (open_nofollow "nofollow.lnk" <> "opened");
    Sys.remove "nofollow.lnk"
  end;
  Sys.remove "nofollow.txt";
  print_endline "OK"
