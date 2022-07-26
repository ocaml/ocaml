(* TEST
   flags = " -w +A-41-42-18"
   * expect
*)
module T1 : sig end = struct
  module M = struct type t end  (* unused type t *)
  open M  (* unused open *)
end;;
[%%expect{|
Line 2, characters 20-26:
2 |   module M = struct type t end  (* unused type t *)
                        ^^^^^^
Warning 34 [unused-type-declaration]: unused type t.
Line 3, characters 2-8:
3 |   open M  (* unused open *)
      ^^^^^^
Warning 33 [unused-open]: unused open M.
module T1 : sig end
|}]


module T2 : sig type s end = struct
  module M = struct type t end
  open M  (* used by line below *)
  type s = t
end;;
[%%expect{|
module T2 : sig type s end
|}]

module T3 : sig end = struct
  type t0 = A  (* unused type and constructor *)
  module M = struct type t = A end
  open M (* used by line below; shadow constructor A *)
  let _ = A (* A belongs to several types *)
end;;
[%%expect{|
Line 4, characters 2-8:
4 |   open M (* used by line below; shadow constructor A *)
      ^^^^^^
Warning 45 [open-shadow-label-constructor]: this open statement shadows the constructor A (which is later used)
Line 2, characters 2-13:
2 |   type t0 = A  (* unused type and constructor *)
      ^^^^^^^^^^^
Warning 34 [unused-type-declaration]: unused type t0.
Line 2, characters 12-13:
2 |   type t0 = A  (* unused type and constructor *)
                ^
Warning 37 [unused-constructor]: unused constructor A.
module T3 : sig end
|}]

module T4 : sig end = struct
  type t0 = A
  module M = struct type t = A end (* unused type and constructor *)
  open M (* unused open; no shadowing (A below refers to the one in t0) *)
  let _ : t0 = A (* disambiguation used *)
end;;
[%%expect{|
Line 3, characters 20-30:
3 |   module M = struct type t = A end (* unused type and constructor *)
                        ^^^^^^^^^^
Warning 34 [unused-type-declaration]: unused type t.
Line 3, characters 29-30:
3 |   module M = struct type t = A end (* unused type and constructor *)
                                 ^
Warning 37 [unused-constructor]: unused constructor A.
Line 4, characters 2-8:
4 |   open M (* unused open; no shadowing (A below refers to the one in t0) *)
      ^^^^^^
Warning 33 [unused-open]: unused open M.
module T4 : sig end
|}]

module T5 : sig end = struct
  type t0 = A (* unused type and constructor *)
  module M = struct type t = A end
  open M (* shadow constructor A *)
  let _ : t = A
end;;
[%%expect{|
Line 4, characters 2-8:
4 |   open M (* shadow constructor A *)
      ^^^^^^
Warning 45 [open-shadow-label-constructor]: this open statement shadows the constructor A (which is later used)
Line 2, characters 2-13:
2 |   type t0 = A (* unused type and constructor *)
      ^^^^^^^^^^^
Warning 34 [unused-type-declaration]: unused type t0.
Line 2, characters 12-13:
2 |   type t0 = A (* unused type and constructor *)
                ^
Warning 37 [unused-constructor]: unused constructor A.
module T5 : sig end
|}]


module T1_bis : sig end = struct
  module M = struct type t end  (* unused type t *)
  open! M  (* unused open *)
end;;
[%%expect{|
Line 2, characters 20-26:
2 |   module M = struct type t end  (* unused type t *)
                        ^^^^^^
Warning 34 [unused-type-declaration]: unused type t.
Line 3, characters 2-9:
3 |   open! M  (* unused open *)
      ^^^^^^^
Warning 66 [unused-open-bang]: unused open! M.
module T1_bis : sig end
|}]

module T2_bis : sig type s end = struct
  module M = struct type t end
  open! M  (* used by line below *)
  type s = t
end;;
[%%expect{|
module T2_bis : sig type s end
|}]

module T3_bis : sig end = struct
  type t0 = A  (* unused type and constructor *)
  module M = struct type t = A end
  open! M (* used by line below; shadow constructor A (disabled) *)
  let _ = A (* A belongs to several types *)
end;;
[%%expect{|
Line 2, characters 2-13:
2 |   type t0 = A  (* unused type and constructor *)
      ^^^^^^^^^^^
Warning 34 [unused-type-declaration]: unused type t0.
Line 2, characters 12-13:
2 |   type t0 = A  (* unused type and constructor *)
                ^
Warning 37 [unused-constructor]: unused constructor A.
module T3_bis : sig end
|}]

module T4_bis : sig end = struct
  type t0 = A
  module M = struct type t = A end (* unused type and constructor *)
  open! M (* unused open; no shadowing (A below refers to the one in t0) *)
  let _ : t0 = A (* disambiguation used *)
end;;
[%%expect{|
Line 3, characters 20-30:
3 |   module M = struct type t = A end (* unused type and constructor *)
                        ^^^^^^^^^^
Warning 34 [unused-type-declaration]: unused type t.
Line 3, characters 29-30:
3 |   module M = struct type t = A end (* unused type and constructor *)
                                 ^
Warning 37 [unused-constructor]: unused constructor A.
Line 4, characters 2-9:
4 |   open! M (* unused open; no shadowing (A below refers to the one in t0) *)
      ^^^^^^^
Warning 66 [unused-open-bang]: unused open! M.
module T4_bis : sig end
|}]

module T5_bis : sig end = struct
  type t0 = A (* unused type and constructor *)
  module M = struct type t = A end
  open! M (* shadow constructor A (disabled) *)
  let _ : t = A
end;;
[%%expect{|
Line 2, characters 2-13:
2 |   type t0 = A (* unused type and constructor *)
      ^^^^^^^^^^^
Warning 34 [unused-type-declaration]: unused type t0.
Line 2, characters 12-13:
2 |   type t0 = A (* unused type and constructor *)
                ^
Warning 37 [unused-constructor]: unused constructor A.
module T5_bis : sig end
|}]

module T7 : sig end = struct
  (* GPR9170 *)
  module M = struct
    class type t = object end
  end
  module type S = sig
    open M
    val f: #t -> unit
  end
  let _ = fun ((module S : S)) -> S.f (object end)
end;;
[%%expect {|
module T7 : sig end
|}]

module T8 : sig end = struct
  (* GPR9170 *)
  module M = struct
    class t = object end
  end
  module type S = sig
    open M
    val f: #t -> unit
  end
  let _ = fun ((module S : S)) -> S.f (object end)
end;;
[%%expect {|
module T8 : sig end
|}]
