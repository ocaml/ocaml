(* TEST_BELOW


*)

type foo =
    Foo: [> `Bla ] as 'b ) * 'b -> foo;;
type foo =
    Foo: 'b * 'b -> foo constraint 'b = [> `Bla ];;

(* TEST
{
  flags = " -w +A -strict-sequence ";
  toplevel;
}
*)
