(* TEST
 flags = "-dshape";
 expect;
*)

(* Everything that couldn't go anywhere else. *)

open struct
  module M = struct
    type t = A
  end
end
[%%expect{|
{}
module M : sig type t = A end
|}]

include M
[%%expect{|
{
 "t"[type] -> {<.0>
               "A"[constructor] -> {<.1>};
               };
 }
type t = M.t = A
|}]

module N = M
[%%expect{|
{
 "N"[module] ->
   Alias(<.3>
         {<.2>
          "t"[type] -> {<.0>
                        "A"[constructor] -> {<.1>};
                        };
          });
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
                  "t"[type] -> {<.4>
                                "A"[constructor] -> {<.5>};
                                };
                  };
 }
module M' : sig type t = A end
|}]

module N' = M'
[%%expect{|
{
 "N'"[module] ->
   Alias(<.7>
         {<.6>
          "t"[type] -> {<.4>
                        "A"[constructor] -> {<.5>};
                        };
          });
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
 "Test"[module] ->
   {<.11>
    "M"[module] -> {<.10>
                    "t"[type] -> {<.8>
                                  "A"[constructor] -> {<.9>};
                                  };
                    };
    };
 }
module Test : sig module M : sig type t = A end end
|}]

include Test
[%%expect{|
{
 "M"[module] -> {<.10>
                 "t"[type] -> {<.8>
                               "A"[constructor] -> {<.9>};
                               };
                 };
 }
module M = Test.M
|}]

module N = M
[%%expect{|
{
 "N"[module] ->
   Alias(<.12>
         {<.10>
          "t"[type] -> {<.8>
                        "A"[constructor] -> {<.9>};
                        };
          });
 }
module N = M
|}]
