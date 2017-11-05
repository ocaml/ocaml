module M: sig
  type t = { alpha: unit; beta: int; delta: unit}
end =
struct
  type t = { alpha: unit; beta:unit; gamma: unit}
end;;
[%%expect {|
Line _, characters 0-60:
Error: Signature mismatch:
       Modules do not match:
         sig type t = { ...; beta : !(unit); !(gamma) : unit; } end
       is not included in
         sig type t = { ...; beta : !(int); !(delta) : unit; } end
       Type declarations do not match:
         type t = { ...; beta : !(unit); !(gamma) : unit; }
       is not included in
         type t = { ...; beta : !(int); !(delta) : unit; }
       The types for field beta are not equal.
|}]

 module M: sig
  type t = { mutable alpha: unit; beta: unit; gamma: unit}
end =
struct
  type t = { alpha: unit; beta:unit; gamma: unit}
end;;
[%%expect {|
Line _, characters 0-60:
Error: Signature mismatch:
       Modules do not match:
         sig type t = { alpha : unit; ... * 2; } end
       is not included in
         sig type t = { !(mutable )alpha : unit; ... * 2; } end
       Type declarations do not match:
         type t = { alpha : unit; ... * 2; }
       is not included in
         type t = { !(mutable )alpha : unit; ... * 2; }
       The mutability of field alpha is different.
|}]
