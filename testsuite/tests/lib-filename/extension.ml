(* TEST
*)

let add_extension () =
  let test f e full =
    assert(Filename.add_extension f e = full);
    assert(Filename.add_extension (Filename.remove_extension full) (Filename.extension full) = full)
  in
  test "" "" "";
  test "foo" "" "foo";
  test "" "txt" "";
  test "foo.txt" "" "foo.txt";
  test "foo.txt" "." "foo.txt";
  test "foo.txt" "gz" "foo.txt.gz";
  test "foo.txt" ".gz" "foo.txt.gz";
  test ".foo" "gz" ".foo.gz";
  test "." "" ".";
  test "." "." ".";
  test "." "txt" ".";
  test "." ".txt" ".";
  test ".." "" "..";
  test ".." "." "..";
  test ".." "txt" "..";
  test ".." ".txt" "..";
  test "foo.txt.gz" "doc" "foo.txt.gz.doc";
  test "foo/" "txt" "foo/"

let with_extension () =
  let test f e full =
    assert(Filename.with_extension f e = full);
    assert(Filename.with_extension (Filename.with_extension f e) e = Filename.with_extension f e)
  in
  test "" "" "";
  test "foo" "" "foo";
  test "" "txt" "";
  test "foo.txt" "" "foo";
  test "foo.txt" "." "foo";
  test "foo.txt" "gz" "foo.gz";
  test "foo.txt" ".gz" "foo.gz";
  test ".foo" "gz" ".foo.gz";
  test "." "" ".";
  test "." "." ".";
  test "." "txt" ".";
  test "." ".txt" ".";
  test ".." "" "..";
  test ".." "." "..";
  test ".." "txt" "..";
  test ".." ".txt" "..";
  test "foo.txt.gz" "doc" "foo.txt.doc";
  test "foo/" "txt" "foo/"

let () =
  let test f e =
    assert(Filename.extension f = e);
    assert(Filename.extension ("foo/" ^ f) = e);
    assert(Filename.with_extension f e = f);
    assert(f = Filename.remove_extension f ^ Filename.extension f)
  in
  test "" "";
  test "foo" "";
  test "foo.txt" ".txt";
  test "foo.txt.gz" ".gz";
  test ".foo" "";
  test "." "";
  test ".." "";
  test "foo..txt" ".txt";
  with_extension ();
  add_extension ()
