(* TEST
*)

let () =
  let test ~suffix name exp =
    let r1 = Filename.chop_suffix_opt ~suffix name <> None in
    let r2 = Filename.check_suffix name suffix in
    assert (r1 = r2);
    assert (r1 = exp)
  in
  let full_test ~suffix name =
    test ~suffix name true;
    match Filename.chop_suffix_opt ~suffix name with
    | None -> assert false
    | Some base -> assert (base ^ suffix = name)
  in
  let win32 = Sys.os_type = "Win32" || Sys.os_type = "Cygwin" in
  full_test ~suffix:".txt" "foo.txt";
  full_test ~suffix:"txt" "foo.txt";
  full_test ~suffix:"" "foo.txt";
  full_test ~suffix:"" "";
  test ~suffix:".txt" "f" false;
  test ~suffix:".txt" "" false;
  test ~suffix:".txt" "foo.txt.bak" false;
  test ~suffix:".txt" "foo.TXT" win32;
  if win32 then
    assert (Filename.chop_suffix_opt ~suffix:".txt" "foo.TXT" = Some "foo")
