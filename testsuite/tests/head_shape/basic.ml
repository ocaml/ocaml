(* TEST
   flags="-dheadshape";
   flat-float-array;
   expect;
*)

type my_int = int
[%%expect{|
shape of my_int: {imm = Any; blocks = [];}
type my_int = int
|}]

type my_bool = bool
[%%expect{|
shape of my_bool: {imm = [0; 1]; blocks = [];}
type my_bool = bool
|}]

type my_string = string
[%%expect{|
shape of my_string: {imm = []; blocks = [string];}
type my_string = string
|}]

type my_float = float
[%%expect{|
shape of my_float: {imm = []; blocks = [double];}
type my_float = float
|}]

type 'a my_array = 'a array
[%%expect{|
shape of _ my_array: {imm = []; blocks = [0; double_array];}
type 'a my_array = 'a array
|}]

type my_floatarray = floatarray
[%%expect{|
shape of my_floatarray: {imm = []; blocks = [double_array];}
type my_floatarray = floatarray
|}]

type my_int32 = int32
[%%expect{|
shape of my_int32: {imm = []; blocks = [custom];}
type my_int32 = int32
|}]

type my_int64 = int64
[%%expect{|
shape of my_int64: {imm = []; blocks = [custom];}
type my_int64 = int64
|}]

type my_nativeint = nativeint
[%%expect{|
shape of my_nativeint: {imm = []; blocks = [custom];}
type my_nativeint = nativeint
|}]

type my_exn = exn
[%%expect{|
shape of my_exn: {imm = []; blocks = [0; object];}
type my_exn = exn
|}]

type 'a my_option = 'a option
[%%expect{|
shape of _ my_option: {imm = [0]; blocks = [0];}
type 'a my_option = 'a option
|}]

type 'a my_list = 'a list
[%%expect{|
shape of _ my_list: {imm = [0]; blocks = [0];}
type 'a my_list = 'a list
|}]

type 'a homemade_option = None | Some of 'a
[%%expect{|
shape of _ homemade_option: {imm = [0]; blocks = [0];}
type 'a homemade_option = None | Some of 'a
|}]
type 'a homemade_list = Nil | Cons of 'a * 'a list
[%%expect{|
shape of _ homemade_list: {imm = [0]; blocks = [0];}
type 'a homemade_list = Nil | Cons of 'a * 'a list
|}]

type abstract
[%%expect{|
shape of abstract: {imm = Any; blocks = Any;}
type abstract
|}]

type lazy_int = int Lazy.t
[%%expect{|
shape of lazy_int: {imm = Any; blocks = [forcing; lazy; forward];}
type lazy_int = int Lazy.t
|}]

type lazy_option = int option Lazy.t
[%%expect{|
shape of lazy_option: {imm = [0]; blocks = [0; forcing; lazy; forward];}
type lazy_option = int option Lazy.t
|}]

type some_object = < x : int; y : int >
[%%expect{|
shape of some_object: {imm = []; blocks = [object];}
type some_object = < x : int; y : int >
|}]
