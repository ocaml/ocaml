(* TEST *)

let ( / ) = Filename.concat

let create_file p =
  Out_channel.with_open_bin p (fun oc -> output_string oc "test")

let create_subtree temp_dir =
  Sys.mkdir (temp_dir / "a") 0o700;
  Sys.mkdir (temp_dir / "a" / "b") 0o700;
  Sys.mkdir (temp_dir / "a" / "b" / "c") 0o700;
  Sys.mkdir (temp_dir / "a" / "b" / "c" / "d") 0o700;
  create_file (temp_dir / "a.txt");
  create_file (temp_dir / "a" / "b.txt");
  create_file (temp_dir / "a" / "b" / "c.txt");
  create_file (temp_dir / "a" / "b" / "c" / "d.txt");
  create_file (temp_dir / "a" / "b" / "c" / "d" / "e.txt")

let () =
  let temp_dir_ref = ref "" in
  begin
    Filename.with_temp_dir "morx" "fleem" @@ fun temp_dir ->
    temp_dir_ref := temp_dir;
    create_subtree temp_dir;
    assert (Sys.filepath_exists (!temp_dir_ref / "a" / "b" / "c" / "d" / "e.txt"));
    assert (Sys.filepath_exists !temp_dir_ref)
  end;
  assert (!temp_dir_ref <> "");
  assert (not (Sys.filepath_exists !temp_dir_ref))

let () =
  let temp_dir_ref = ref "" in
  begin match
    Filename.with_temp_dir "morx" "fleem" @@ fun temp_dir ->
    temp_dir_ref := temp_dir;
    create_subtree temp_dir;
    assert (Sys.filepath_exists (!temp_dir_ref / "a" / "b" / "c" / "d" / "e.txt"));
    assert (Sys.filepath_exists !temp_dir_ref);
    failwith "test"
  with
  | () -> assert false
  | exception _ -> ()
  end;
  assert (!temp_dir_ref <> "");
  assert (not (Sys.filepath_exists !temp_dir_ref))
