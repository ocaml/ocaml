(* TEST *)

let ( / ) = Filename.concat

let remove_if_exists p =
  if Sys.filepath_exists p then Sys.remove p

let rmdir_if_exists p =
  if Sys.filepath_exists p then Sys.rmdir p

let create_file p =
  Out_channel.with_open_bin p (fun oc -> output_string oc "test")

let () =
  let tmpdir = Filename.temp_dir "morx" "fleem" in
  Fun.protect ~finally:begin fun () ->
    remove_if_exists (tmpdir / "a" / "b" / "c" / "d" / "e.txt");
    remove_if_exists (tmpdir / "a" / "b" / "c" / "d.txt");
    remove_if_exists (tmpdir / "a" / "b" / "c.txt");
    remove_if_exists (tmpdir / "a" / "b.txt");
    remove_if_exists (tmpdir / "a.txt");
    rmdir_if_exists (tmpdir / "a" / "b" / "c" / "d");
    rmdir_if_exists (tmpdir / "a" / "b" / "c");
    rmdir_if_exists (tmpdir / "a" / "b");
    rmdir_if_exists (tmpdir / "a");
    rmdir_if_exists tmpdir
  end @@ fun () ->
  Sys.mkdir (tmpdir / "a") 0o700;
  Sys.mkdir (tmpdir / "a" / "b") 0o700;
  Sys.mkdir (tmpdir / "a" / "b" / "c") 0o700;
  Sys.mkdir (tmpdir / "a" / "b" / "c" / "d") 0o700;
  create_file (tmpdir / "a.txt");
  create_file (tmpdir / "a" / "b.txt");
  create_file (tmpdir / "a" / "b" / "c.txt");
  create_file (tmpdir / "a" / "b" / "c" / "d.txt");
  create_file (tmpdir / "a" / "b" / "c" / "d" / "e.txt");
  assert (Sys.filepath_exists (tmpdir / "a" / "b" / "c" / "d" / "e.txt"));
  assert (Sys.filepath_exists tmpdir);
  Filename.rmtree (tmpdir / "a");
  assert (not (Sys.filepath_exists (tmpdir / "a")));
  assert (Sys.filepath_exists (tmpdir / "a.txt"));
  assert (Sys.filepath_exists tmpdir);
  Filename.rmtree (tmpdir / "a" / "b");
  assert (not (Sys.filepath_exists (tmpdir / "a" / "b")));
  assert (not (Sys.filepath_exists (tmpdir / "a")));
  assert (Sys.filepath_exists (tmpdir / "a.txt"));
  assert (Sys.filepath_exists tmpdir);
  Filename.rmtree (tmpdir / "a");
  assert (not (Sys.filepath_exists (tmpdir / "a" / "b")));
  assert (not (Sys.filepath_exists (tmpdir / "a")));
  assert (Sys.filepath_exists (tmpdir / "a.txt"));
  assert (Sys.filepath_exists tmpdir);
  Filename.rmtree tmpdir;
  assert (not (Sys.filepath_exists (tmpdir / "a" / "b" / "c" / "d" / "e.txt")));
  assert (not (Sys.filepath_exists (tmpdir / "a" / "b")));
  assert (not (Sys.filepath_exists (tmpdir / "a")));
  assert (not (Sys.filepath_exists (tmpdir / "a.txt")));
  assert (not (Sys.filepath_exists tmpdir));
