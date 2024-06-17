(* TEST

 set x = "a";
 set expected = "a";
 script = "bash -c 'if [[ $x != $expected ]]; then exit 1; fi'";
 script;
 {
  expected = "a";
  script;
 }{
  x = "b";
  expected = "b";
  script;
 }{
  expected = "a";
  script;
 }
 expected = "a";
 script;
 x = "c";
 expected = "c";
 script;
 {
  expected = "c";
  script;
 }{
  x = "d";
  expected = "d";
  script;
 }{
  expected = "c";
  script;
 }
 expected = "c";
 script;
*)
