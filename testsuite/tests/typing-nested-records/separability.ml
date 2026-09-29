(* TEST
   flat-float-array;
   expect;
*)

type 'a outer = {
  inner : { value : 'a };
}

type packed = Pack : 'a outer.inner -> packed [@@unboxed]
[%%expect{|
type 'a outer = { inner : { value : 'a; }; }
type packed = Pack : 'a outer.inner -> packed [@@unboxed]
|}]

type 'a deep = {
  outer : { inner : { value : 'a } };
}

type deep_packed = Deep_pack : 'a deep.outer.inner -> deep_packed [@@unboxed]
[%%expect{|
type 'a deep = { outer : { inner : { value : 'a; }; }; }
type deep_packed = Deep_pack : 'a deep.outer.inner -> deep_packed [@@unboxed]
|}]
