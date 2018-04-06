(* TEST
   * expect
*)

module rec A: sig val x: int end = struct let x = B.x end
and B:sig val x: int end = struct let x = E.y end
and C:sig val x: int end = struct let x = B.x end
and D:sig val x: int end = struct let x = C.x end
and E:sig val x: int val y:int end = struct let x = D.x let y = 0 end
[%%expect {|
Line _, characters 27-49:
  and B:sig val x: int end = struct let x = E.y end
                             ^^^^^^^^^^^^^^^^^^^^^^
Error: Cannot safely evaluate the definition of the following cycle
       of recursively-defined modules: B -> E -> D -> C -> B.
       There are no safe modules in this cycle (see manual section 8.4)
|}]

module rec M: sig val f: unit -> int end = struct let f () = N.x end
and N:sig val x: int end = struct let x = M.f () end;;
[%%expect {|
Exception: Undefined_recursive_module ("", 1, 43).
|}]

