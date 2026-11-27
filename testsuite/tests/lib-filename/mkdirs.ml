(* TEST *)

let ( / ) = Filename.concat

let rmdir_if_exists p =
  if Sys.filepath_exists p then Sys.rmdir p

let () =
  let tmpdir = Filename.temp_dir "morx" "fleem" in
  Fun.protect ~finally:begin fun () ->
    rmdir_if_exists (tmpdir / "a" / "b" / "c" / "d");
    rmdir_if_exists (tmpdir / "a" / "b" / "c");
    rmdir_if_exists (tmpdir / "a" / "b");
    rmdir_if_exists (tmpdir / "a");
    rmdir_if_exists tmpdir
  end @@ fun () ->
  let deep_subdir = tmpdir / "a" / "b" / "c" / "d" in
  Filename.mkdirs deep_subdir 0o700;
  assert (Sys.is_directory deep_subdir);
  begin match Filename.mkdirs ~exists_ok:false deep_subdir 0o700 with
  | () -> assert false
  | exception _ -> ()
  end;
  Filename.mkdirs ~exists_ok:true deep_subdir 0o700;
  Filename.mkdirs deep_subdir 0o700
