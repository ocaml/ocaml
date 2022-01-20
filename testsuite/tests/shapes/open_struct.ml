(* TEST
   flags = "-dshape"
   * expect
*)

(* Everything that couldn't go anywhere else. *)

open struct
  module M = struct
    type t = A
  end
end
[%%expect{|
{
 }
module M : sig type t = A end
|}]

include M
[%%expect{|
{
 "t"[type] -> <.0>;
 }
type t = M.t = A
|}]

module N = M
[%%expect{|
{
 "N"[module] -> {<.2>
                 "t"[type] -> <.0>;
                 };
 }
module N = M
|}]

(* Not open structs, but the code handling the following is currently very
   similar to the one for open struct (i.e. calls [Env.enter_signature]), and
   so we are likely to encounter the same bugs, if any. *)

include struct
  module M' = struct
    type t = A
  end
end
[%%expect{|
{
 "M'"[module] -> {<.6>
                  "t"[type] -> <.4>;
                  };
 }
module M' : sig type t = A end
|}]

module N' = M'
[%%expect{|
{
 "N'"[module] -> {<.6>
                  "t"[type] -> <.4>;
                  };
 }
module N' = M'
|}]

module Test = struct
  module M = struct
    type t = A
  end
end
[%%expect{|
{
 "Test"[module] -> {<.11>
                    "M"[module] -> {<.10>
                                    "t"[type] -> <.8>;
                                    };
                    };
 }
module Test : sig module M : sig type t = A end end
|}]

include Test
[%%expect{|
{
 "M"[module] -> {<.10>
                 "t"[type] -> <.8>;
                 };
 }
module M = Test.M
|}]

module N = M
[%%expect{|
{
 "N"[module] -> {<.10>
                 "t"[type] -> <.8>;
                 };
 }
module N = M
|}]
