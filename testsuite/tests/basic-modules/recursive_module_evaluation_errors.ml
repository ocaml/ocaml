(* TEST
   * expect
*)

module rec A: sig val x: int end = struct let x = B.y end
and B:sig val x: int val y:int end = struct let x = C.x let y = 0 end
and C:sig val x: int end = struct let x = B.y end ;;
[%%expect {|
Line _, characters 37-69:
  and B:sig val x: int val y:int end = struct let x = C.x let y = 0 end
                                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Cannot safely evaluate the definition of the following cycle
       of recursively-defined modules: B -> C -> B.
       There are no safe modules in this cycle (see manual section 8.4)
|}]

module rec M: sig val f: unit -> int end = struct let f () = N.x end
and N:sig val x: int end = struct let x = M.f () end;;
[%%expect {|
Exception: Undefined_recursive_module ("", 1, 43).
|}]

