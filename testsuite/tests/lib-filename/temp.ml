(* TEST *)

let () =
 let tmpdir1 = ref "" in
 let tmpdir2 = ref "" in
 Fun.protect ~finally:(fun () ->
   if !tmpdir1 <> "" then Sys.rmdir !tmpdir1;
   if !tmpdir2 <> "" then Sys.rmdir !tmpdir2;
 ) (fun () ->
    tmpdir1 := Filename.temp_dir "morx" "fleem";
    assert (Sys.is_directory !tmpdir1);
    tmpdir2 := Filename.temp_dir "morx" "fleem" ;
    assert (Sys.is_directory !tmpdir2);
    assert (!tmpdir1 <> !tmpdir2);
   )
