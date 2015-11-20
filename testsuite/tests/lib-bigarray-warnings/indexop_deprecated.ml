
module Defs = struct
  open Bigarray

  let c_float f = f float32 c_layout

  let a_1 =
    let a = c_float Array1.create 1 in
    a.{0} <- 1.;
    a

  let a_2 =
    let a = c_float Array2.create 1 1 in
    a.{0,0} <- 1.;
    a

  let a_3 =
    let a = c_float Array3.create 1 1 1 in
    a.{0,0,0} <- 1.;
    a

  let a_gen =
    let a = c_float Genarray.create [|1;1;1;1|] in
    a.{0,0,0,0} <- 1.;
    a

end
open Defs

(* Deprecated use of .{} when they are not in scope *)
let test =
    a_1.{0} <- a_1.{0}
  ; a_2.{0,0} <- a_2.{0,0}
  ; a_3.{0,0,0} <- a_3.{0,0,0}
  ; a_gen.{0,0,0,0} <- a_gen.{0,0,0,0}

module Explicit_bigarray = struct
  open Bigarray

(* Bigarray.{} operators *)
let test =
    a_1.{0} <- a_1.{0}
  ; a_2.{0,0} <- a_2.{0,0}
  ; a_3.{0,0,0} <- a_3.{0,0,0}
  ; a_gen.{0,0,0,0} <- a_gen.{0,0,0,0}
end

module User_defined = struct
  let ( .{} )   _a _k = ()
  let ( .{}<- ) _a _k _x = ()
  let ( .{,} )   _a _k _l = ()
  let ( .{,}<- ) _a _k _l _x = ()
  let ( .{,,} )   _a _k _l _m = ()
  let ( .{,,}<- ) _a _k _l _m _x = ()
  let ( .{,..,} )   _a _ks = ()
  let ( .{,..,}<- ) _a _ks _x = ()

(* User defined .{} operators *)
let test =
    a_1.{0} <- a_1.{0}
  ; a_2.{0,0} <- a_2.{0,0}
  ; a_3.{0,0,0} <- a_3.{0,0,0}
  ; a_gen.{0,0,0,0} <- a_gen.{0,0,0,0}
end
