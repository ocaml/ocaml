(* TEST
 expect;
*)

(* Basics *)

type 'a state = effect
  | Get : 'a
  | Set : 'a -> unit
[%%expect {|
type 'a state = effect Get : 'a | Set : 'a -> unit
|}]

let g = Get
[%%expect {|
val g : ('a, 'a state) operation = Get
|}]

let s = Set
[%%expect {|
Line 1, characters 8-11:
1 | let s = Set
            ^^^
Error: The constructor "Set" expects 1 argument(s),
       but is applied here to 0 argument(s)
|}]

let s = Set 5
[%%expect {|
val s : (unit, int state) operation = Set 5
|}]

let foo = function
  | Get -> true
  | Set _ -> false
[%%expect {|
val foo : (unit, unit state) operation -> bool = <fun>
|}]

(* Indexing *)

let foo (type a) : (a, int state) operation -> int = function
  | Get -> 7
  | Set x -> x
[%%expect {|
val foo : ('a, int state) operation -> int = <fun>
|}]

let foo (type a) : (a, int state) operation -> a = function
  | Get -> 0
  | Set _ -> ()
[%%expect {|
val foo : ('a, int state) operation -> 'a = <fun>
|}]

type 'a reader = effect
  | Read : 'a

let foo (type a b) : (a, b) operation -> string = function
  | Read -> "a"
  | Set _ -> "c"
[%%expect {|
type 'a reader = effect Read : 'a
Line 5, characters 4-8:
5 |   | Read -> "a"
        ^^^^
Error: This pattern matches values of type "(a, 'a reader) operation"
       but a pattern was expected which matches values of type
         "(a, b) operation"
       Type "'a reader" is not compatible with type "b"
|}]


(* Disambiguation *)

module Writer = struct

  type 'a t = effect
    | Write : 'a -> unit

end
[%%expect {|
module Writer : sig type 'a t = effect Write : 'a -> unit end
|}]

let x = Write 5
[%%expect {|
Line 1, characters 8-13:
1 | let x = Write 5
            ^^^^^
Error: Unbound constructor "Write"
|}]

let x : (_, _ Writer.t) operation = Write 5
[%%expect {|
val x : (unit, int Writer.t) operation = Writer.Write 5
|}]

let foo = function Write _ -> ()
[%%expect {|
Line 1, characters 19-24:
1 | let foo = function Write _ -> ()
                       ^^^^^
Error: Unbound constructor "Write"
|}]

let foo : (_, _ Writer.t) operation -> unit = function Write _ -> ()
[%%expect {|
val foo : (unit, 'a Writer.t) operation -> unit = <fun>
|}]

(* Exhaustiveness *)

let foo (type a) : (a, int state) operation -> int = function
  | Get -> 7
[%%expect {|
Lines 1-2, characters 53-12:
1 | .....................................................function
2 |   | Get -> 7
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
Set _

val foo : ('a, int state) operation -> int = <fun>
|}]

(* Existentials *)

type 'a ext = effect
  Ext : 'b * ('b -> 'a) -> 'a
[%%expect {|
type 'a ext = effect Ext : 'b * ('b -> 'a) -> 'a
|}]

let apply = function
  | Ext(x, f) -> f x
[%%expect {|
val apply : ('a, 'a ext) operation -> 'a = <fun>
|}]

let apply_annots = function
  | Ext (type a) (x, f : a * (a -> _)) -> f x
[%%expect {|
val apply_annots : ('a, 'a ext) operation -> 'a = <fun>
|}]

let leak = function
  | Ext(x, _) -> x
[%%expect {|
Line 2, characters 17-18:
2 |   | Ext(x, _) -> x
                     ^
Error: This expression has type "$b" but an expression was expected of type "'a"
       The type constructor "$b" would escape its scope
       Hint: "$b" is an existential type bound by the constructor "Ext".
|}]

(* Inline records *)

type 'a t = effect
  | Foo : { bar : 'a; baz : int } -> float
[%%expect {|
type 'a t = effect Foo : { bar : 'a; baz : int; } -> float
|}]

let x = Foo { bar = 7; baz = 10 }
[%%expect {|
val x : (float, int t) operation = Foo {bar = 7; baz = 10}
|}]

let update = function
  | Foo r -> Foo { r with bar = "five" }
[%%expect {|
val update : (float, 'a t) operation -> (float, string t) operation = <fun>
|}]

let read = function
  | Foo r -> r.bar, r.baz
[%%expect {|
val read : (float, 'a t) operation -> 'a * int = <fun>
|}]

let read = function
  | Foo r -> r
[%%expect {|
Line 2, characters 13-14:
2 |   | Foo r -> r
                 ^
Error: This form is not allowed as the type of the inlined record could escape.
|}]

let read = function
  | Foo { bar; baz } -> bar, baz
[%%expect {|
val read : (float, 'a t) operation -> 'a * int = <fun>
|}]

(* Declaration errors *)

type t = effect
  | Foo : 'a * 'b -> int
[%%expect {|
type t = effect Foo : 'a * 'b -> int
|}]

type t = effect
  | Foo : 'b. 'a * 'b -> int
[%%expect {|
Line 2, characters 14-16:
2 |   | Foo : 'b. 'a * 'b -> int
                  ^^
Error: The type variable "'a" is unbound in this type declaration.
|}]

type 'a t = effect
  | Foo : 'a * 'b -> int
[%%expect {|
type 'a t = effect Foo : 'a * 'b -> int
|}]

type 'a t = effect
  | Foo : 'b. 'a * 'b -> int
[%%expect {|
type 'a t = effect Foo : 'a * 'b -> int
|}]

type a = effect
  | A : int -> int
[%%expect {|
type a = effect A : int -> int
|}]

type b = a = effect
  | B : int -> int
[%%expect {|
Lines 1-2, characters 0-18:
1 | type b = a = effect
2 |   | B : int -> int
Error: This effect definition does not match that of type "a"
       Operations have different names, A and B.
|}]

type too_big = effect
  | O0 : int -> int | O1 : int -> int | O2 : int -> int
  | O3 : int -> int | O4 : int -> int | O5 : int -> int
  | O6 : int -> int | O7 : int -> int | O8 : int -> int
  | O9 : int -> int | O10 : int -> int | O11 : int -> int
  | O12 : int -> int | O13 : int -> int | O14 : int -> int
  | O15 : int -> int | O16 : int -> int | O17 : int -> int
  | O18 : int -> int | O19 : int -> int | O20 : int -> int
  | O21 : int -> int | O22 : int -> int | O23 : int -> int
  | O24 : int -> int | O25 : int -> int | O26 : int -> int
  | O27 : int -> int | O28 : int -> int | O29 : int -> int
  | O30 : int -> int | O31 : int -> int | O32 : int -> int
  | O33 : int -> int | O34 : int -> int | O35 : int -> int
  | O36 : int -> int | O37 : int -> int | O38 : int -> int
  | O39 : int -> int | O40 : int -> int | O41 : int -> int
  | O42 : int -> int | O43 : int -> int | O44 : int -> int
  | O45 : int -> int | O46 : int -> int | O47 : int -> int
  | O48 : int -> int | O49 : int -> int | O50 : int -> int
  | O51 : int -> int | O52 : int -> int | O53 : int -> int
  | O54 : int -> int | O55 : int -> int | O56 : int -> int
  | O57 : int -> int | O58 : int -> int | O59 : int -> int
  | O60 : int -> int | O61 : int -> int | O62 : int -> int
  | O63 : int -> int | O64 : int -> int | O65 : int -> int
  | O66 : int -> int | O67 : int -> int | O68 : int -> int
  | O69 : int -> int | O70 : int -> int | O71 : int -> int
  | O72 : int -> int | O73 : int -> int | O74 : int -> int
  | O75 : int -> int | O76 : int -> int | O77 : int -> int
  | O78 : int -> int | O79 : int -> int | O80 : int -> int
  | O81 : int -> int | O82 : int -> int | O83 : int -> int
  | O84 : int -> int | O85 : int -> int | O86 : int -> int
  | O87 : int -> int | O88 : int -> int | O89 : int -> int
  | O90 : int -> int | O91 : int -> int | O92 : int -> int
  | O93 : int -> int | O94 : int -> int | O95 : int -> int
  | O96 : int -> int | O97 : int -> int | O98 : int -> int
  | O99 : int -> int | O100 : int -> int | O101 : int -> int
  | O102 : int -> int | O103 : int -> int | O104 : int -> int
  | O105 : int -> int | O106 : int -> int | O107 : int -> int
  | O108 : int -> int | O109 : int -> int | O110 : int -> int
  | O111 : int -> int | O112 : int -> int | O113 : int -> int
  | O114 : int -> int | O115 : int -> int | O116 : int -> int
  | O117 : int -> int | O118 : int -> int | O119 : int -> int
  | O120 : int -> int | O121 : int -> int | O122 : int -> int
  | O123 : int -> int | O124 : int -> int | O125 : int -> int
  | O126 : int -> int | O127 : int -> int | O128 : int -> int
  | O129 : int -> int | O130 : int -> int | O131 : int -> int
  | O132 : int -> int | O133 : int -> int | O134 : int -> int
  | O135 : int -> int | O136 : int -> int | O137 : int -> int
  | O138 : int -> int | O139 : int -> int | O140 : int -> int
  | O141 : int -> int | O142 : int -> int | O143 : int -> int
  | O144 : int -> int | O145 : int -> int | O146 : int -> int
  | O147 : int -> int | O148 : int -> int | O149 : int -> int
  | O150 : int -> int | O151 : int -> int | O152 : int -> int
  | O153 : int -> int | O154 : int -> int | O155 : int -> int
  | O156 : int -> int | O157 : int -> int | O158 : int -> int
  | O159 : int -> int | O160 : int -> int | O161 : int -> int
  | O162 : int -> int | O163 : int -> int | O164 : int -> int
  | O165 : int -> int | O166 : int -> int | O167 : int -> int
  | O168 : int -> int | O169 : int -> int | O170 : int -> int
  | O171 : int -> int | O172 : int -> int | O173 : int -> int
  | O174 : int -> int | O175 : int -> int | O176 : int -> int
  | O177 : int -> int | O178 : int -> int | O179 : int -> int
  | O180 : int -> int | O181 : int -> int | O182 : int -> int
  | O183 : int -> int | O184 : int -> int | O185 : int -> int
  | O186 : int -> int | O187 : int -> int | O188 : int -> int
  | O189 : int -> int | O190 : int -> int | O191 : int -> int
  | O192 : int -> int | O193 : int -> int | O194 : int -> int
  | O195 : int -> int | O196 : int -> int | O197 : int -> int
  | O198 : int -> int | O199 : int -> int | O200 : int -> int
  | O201 : int -> int | O202 : int -> int | O203 : int -> int
  | O204 : int -> int | O205 : int -> int | O206 : int -> int
  | O207 : int -> int | O208 : int -> int | O209 : int -> int
  | O210 : int -> int | O211 : int -> int | O212 : int -> int
  | O213 : int -> int | O214 : int -> int | O215 : int -> int
  | O216 : int -> int | O217 : int -> int | O218 : int -> int
  | O219 : int -> int | O220 : int -> int | O221 : int -> int
  | O222 : int -> int | O223 : int -> int | O224 : int -> int
  | O225 : int -> int | O226 : int -> int | O227 : int -> int
  | O228 : int -> int | O229 : int -> int | O230 : int -> int
  | O231 : int -> int | O232 : int -> int | O233 : int -> int
  | O234 : int -> int | O235 : int -> int | O236 : int -> int
  | O237 : int -> int | O238 : int -> int | O239 : int -> int
  | O240 : int -> int | O241 : int -> int | O242 : int -> int
  | O243 : int -> int | O244 : int -> int
[%%expect {|
Lines 1-83, characters 0-41:
 1 | type too_big = effect
 2 |   | O0 : int -> int | O1 : int -> int | O2 : int -> int
 3 |   | O3 : int -> int | O4 : int -> int | O5 : int -> int
 4 |   | O6 : int -> int | O7 : int -> int | O8 : int -> int
 5 |   | O9 : int -> int | O10 : int -> int | O11 : int -> int
...
80 |   | O234 : int -> int | O235 : int -> int | O236 : int -> int
81 |   | O237 : int -> int | O238 : int -> int | O239 : int -> int
82 |   | O240 : int -> int | O241 : int -> int | O242 : int -> int
83 |   | O243 : int -> int | O244 : int -> int
Error: Too many non-constant operations
       -- maximum is 244 non-constant operations
|}]

type biggest_allowed = effect
  | O0 : int -> int | O1 : int -> int | O2 : int -> int
  | O3 : int -> int | O4 : int -> int | O5 : int -> int
  | O6 : int -> int | O7 : int -> int | O8 : int -> int
  | O9 : int -> int | O10 : int -> int | O11 : int -> int
  | O12 : int -> int | O13 : int -> int | O14 : int -> int
  | O15 : int -> int | O16 : int -> int | O17 : int -> int
  | O18 : int -> int | O19 : int -> int | O20 : int -> int
  | O21 : int -> int | O22 : int -> int | O23 : int -> int
  | O24 : int -> int | O25 : int -> int | O26 : int -> int
  | O27 : int -> int | O28 : int -> int | O29 : int -> int
  | O30 : int -> int | O31 : int -> int | O32 : int -> int
  | O33 : int -> int | O34 : int -> int | O35 : int -> int
  | O36 : int -> int | O37 : int -> int | O38 : int -> int
  | O39 : int -> int | O40 : int -> int | O41 : int -> int
  | O42 : int -> int | O43 : int -> int | O44 : int -> int
  | O45 : int -> int | O46 : int -> int | O47 : int -> int
  | O48 : int -> int | O49 : int -> int | O50 : int -> int
  | O51 : int -> int | O52 : int -> int | O53 : int -> int
  | O54 : int -> int | O55 : int -> int | O56 : int -> int
  | O57 : int -> int | O58 : int -> int | O59 : int -> int
  | O60 : int -> int | O61 : int -> int | O62 : int -> int
  | O63 : int -> int | O64 : int -> int | O65 : int -> int
  | O66 : int -> int | O67 : int -> int | O68 : int -> int
  | O69 : int -> int | O70 : int -> int | O71 : int -> int
  | O72 : int -> int | O73 : int -> int | O74 : int -> int
  | O75 : int -> int | O76 : int -> int | O77 : int -> int
  | O78 : int -> int | O79 : int -> int | O80 : int -> int
  | O81 : int -> int | O82 : int -> int | O83 : int -> int
  | O84 : int -> int | O85 : int -> int | O86 : int -> int
  | O87 : int -> int | O88 : int -> int | O89 : int -> int
  | O90 : int -> int | O91 : int -> int | O92 : int -> int
  | O93 : int -> int | O94 : int -> int | O95 : int -> int
  | O96 : int -> int | O97 : int -> int | O98 : int -> int
  | O99 : int -> int | O100 : int -> int | O101 : int -> int
  | O102 : int -> int | O103 : int -> int | O104 : int -> int
  | O105 : int -> int | O106 : int -> int | O107 : int -> int
  | O108 : int -> int | O109 : int -> int | O110 : int -> int
  | O111 : int -> int | O112 : int -> int | O113 : int -> int
  | O114 : int -> int | O115 : int -> int | O116 : int -> int
  | O117 : int -> int | O118 : int -> int | O119 : int -> int
  | O120 : int -> int | O121 : int -> int | O122 : int -> int
  | O123 : int -> int | O124 : int -> int | O125 : int -> int
  | O126 : int -> int | O127 : int -> int | O128 : int -> int
  | O129 : int -> int | O130 : int -> int | O131 : int -> int
  | O132 : int -> int | O133 : int -> int | O134 : int -> int
  | O135 : int -> int | O136 : int -> int | O137 : int -> int
  | O138 : int -> int | O139 : int -> int | O140 : int -> int
  | O141 : int -> int | O142 : int -> int | O143 : int -> int
  | O144 : int -> int | O145 : int -> int | O146 : int -> int
  | O147 : int -> int | O148 : int -> int | O149 : int -> int
  | O150 : int -> int | O151 : int -> int | O152 : int -> int
  | O153 : int -> int | O154 : int -> int | O155 : int -> int
  | O156 : int -> int | O157 : int -> int | O158 : int -> int
  | O159 : int -> int | O160 : int -> int | O161 : int -> int
  | O162 : int -> int | O163 : int -> int | O164 : int -> int
  | O165 : int -> int | O166 : int -> int | O167 : int -> int
  | O168 : int -> int | O169 : int -> int | O170 : int -> int
  | O171 : int -> int | O172 : int -> int | O173 : int -> int
  | O174 : int -> int | O175 : int -> int | O176 : int -> int
  | O177 : int -> int | O178 : int -> int | O179 : int -> int
  | O180 : int -> int | O181 : int -> int | O182 : int -> int
  | O183 : int -> int | O184 : int -> int | O185 : int -> int
  | O186 : int -> int | O187 : int -> int | O188 : int -> int
  | O189 : int -> int | O190 : int -> int | O191 : int -> int
  | O192 : int -> int | O193 : int -> int | O194 : int -> int
  | O195 : int -> int | O196 : int -> int | O197 : int -> int
  | O198 : int -> int | O199 : int -> int | O200 : int -> int
  | O201 : int -> int | O202 : int -> int | O203 : int -> int
  | O204 : int -> int | O205 : int -> int | O206 : int -> int
  | O207 : int -> int | O208 : int -> int | O209 : int -> int
  | O210 : int -> int | O211 : int -> int | O212 : int -> int
  | O213 : int -> int | O214 : int -> int | O215 : int -> int
  | O216 : int -> int | O217 : int -> int | O218 : int -> int
  | O219 : int -> int | O220 : int -> int | O221 : int -> int
  | O222 : int -> int | O223 : int -> int | O224 : int -> int
  | O225 : int -> int | O226 : int -> int | O227 : int -> int
  | O228 : int -> int | O229 : int -> int | O230 : int -> int
  | O231 : int -> int | O232 : int -> int | O233 : int -> int
  | O234 : int -> int | O235 : int -> int | O236 : int -> int
  | O237 : int -> int | O238 : int -> int | O239 : int -> int
  | O240 : int -> int | O241 : int -> int | O242 : int -> int
  | O243 : int -> int
[%%expect {|
type biggest_allowed = effect
    O0 : int -> int
  | O1 : int -> int
  | O2 : int -> int
  | O3 : int -> int
  | O4 : int -> int
  | O5 : int -> int
  | O6 : int -> int
  | O7 : int -> int
  | O8 : int -> int
  | O9 : int -> int
  | O10 : int -> int
  | O11 : int -> int
  | O12 : int -> int
  | O13 : int -> int
  | O14 : int -> int
  | O15 : int -> int
  | O16 : int -> int
  | O17 : int -> int
  | O18 : int -> int
  | O19 : int -> int
  | O20 : int -> int
  | O21 : int -> int
  | O22 : int -> int
  | O23 : int -> int
  | O24 : int -> int
  | O25 : int -> int
  | O26 : int -> int
  | O27 : int -> int
  | O28 : int -> int
  | O29 : int -> int
  | O30 : int -> int
  | O31 : int -> int
  | O32 : int -> int
  | O33 : int -> int
  | O34 : int -> int
  | O35 : int -> int
  | O36 : int -> int
  | O37 : int -> int
  | O38 : int -> int
  | O39 : int -> int
  | O40 : int -> int
  | O41 : int -> int
  | O42 : int -> int
  | O43 : int -> int
  | O44 : int -> int
  | O45 : int -> int
  | O46 : int -> int
  | O47 : int -> int
  | O48 : int -> int
  | O49 : int -> int
  | O50 : int -> int
  | O51 : int -> int
  | O52 : int -> int
  | O53 : int -> int
  | O54 : int -> int
  | O55 : int -> int
  | O56 : int -> int
  | O57 : int -> int
  | O58 : int -> int
  | O59 : int -> int
  | O60 : int -> int
  | O61 : int -> int
  | O62 : int -> int
  | O63 : int -> int
  | O64 : int -> int
  | O65 : int -> int
  | O66 : int -> int
  | O67 : int -> int
  | O68 : int -> int
  | O69 : int -> int
  | O70 : int -> int
  | O71 : int -> int
  | O72 : int -> int
  | O73 : int -> int
  | O74 : int -> int
  | O75 : int -> int
  | O76 : int -> int
  | O77 : int -> int
  | O78 : int -> int
  | O79 : int -> int
  | O80 : int -> int
  | O81 : int -> int
  | O82 : int -> int
  | O83 : int -> int
  | O84 : int -> int
  | O85 : int -> int
  | O86 : int -> int
  | O87 : int -> int
  | O88 : int -> int
  | O89 : int -> int
  | O90 : int -> int
  | O91 : int -> int
  | O92 : int -> int
  | O93 : int -> int
  | O94 : int -> int
  | O95 : int -> int
  | O96 : int -> int
  | O97 : int -> int
  | O98 : int -> int
  | O99 : int -> int
  | O100 : int -> int
  | O101 : int -> int
  | O102 : int -> int
  | O103 : int -> int
  | O104 : int -> int
  | O105 : int -> int
  | O106 : int -> int
  | O107 : int -> int
  | O108 : int -> int
  | O109 : int -> int
  | O110 : int -> int
  | O111 : int -> int
  | O112 : int -> int
  | O113 : int -> int
  | O114 : int -> int
  | O115 : int -> int
  | O116 : int -> int
  | O117 : int -> int
  | O118 : int -> int
  | O119 : int -> int
  | O120 : int -> int
  | O121 : int -> int
  | O122 : int -> int
  | O123 : int -> int
  | O124 : int -> int
  | O125 : int -> int
  | O126 : int -> int
  | O127 : int -> int
  | O128 : int -> int
  | O129 : int -> int
  | O130 : int -> int
  | O131 : int -> int
  | O132 : int -> int
  | O133 : int -> int
  | O134 : int -> int
  | O135 : int -> int
  | O136 : int -> int
  | O137 : int -> int
  | O138 : int -> int
  | O139 : int -> int
  | O140 : int -> int
  | O141 : int -> int
  | O142 : int -> int
  | O143 : int -> int
  | O144 : int -> int
  | O145 : int -> int
  | O146 : int -> int
  | O147 : int -> int
  | O148 : int -> int
  | O149 : int -> int
  | O150 : int -> int
  | O151 : int -> int
  | O152 : int -> int
  | O153 : int -> int
  | O154 : int -> int
  | O155 : int -> int
  | O156 : int -> int
  | O157 : int -> int
  | O158 : int -> int
  | O159 : int -> int
  | O160 : int -> int
  | O161 : int -> int
  | O162 : int -> int
  | O163 : int -> int
  | O164 : int -> int
  | O165 : int -> int
  | O166 : int -> int
  | O167 : int -> int
  | O168 : int -> int
  | O169 : int -> int
  | O170 : int -> int
  | O171 : int -> int
  | O172 : int -> int
  | O173 : int -> int
  | O174 : int -> int
  | O175 : int -> int
  | O176 : int -> int
  | O177 : int -> int
  | O178 : int -> int
  | O179 : int -> int
  | O180 : int -> int
  | O181 : int -> int
  | O182 : int -> int
  | O183 : int -> int
  | O184 : int -> int
  | O185 : int -> int
  | O186 : int -> int
  | O187 : int -> int
  | O188 : int -> int
  | O189 : int -> int
  | O190 : int -> int
  | O191 : int -> int
  | O192 : int -> int
  | O193 : int -> int
  | O194 : int -> int
  | O195 : int -> int
  | O196 : int -> int
  | O197 : int -> int
  | O198 : int -> int
  | O199 : int -> int
  | O200 : int -> int
  | O201 : int -> int
  | O202 : int -> int
  | O203 : int -> int
  | O204 : int -> int
  | O205 : int -> int
  | O206 : int -> int
  | O207 : int -> int
  | O208 : int -> int
  | O209 : int -> int
  | O210 : int -> int
  | O211 : int -> int
  | O212 : int -> int
  | O213 : int -> int
  | O214 : int -> int
  | O215 : int -> int
  | O216 : int -> int
  | O217 : int -> int
  | O218 : int -> int
  | O219 : int -> int
  | O220 : int -> int
  | O221 : int -> int
  | O222 : int -> int
  | O223 : int -> int
  | O224 : int -> int
  | O225 : int -> int
  | O226 : int -> int
  | O227 : int -> int
  | O228 : int -> int
  | O229 : int -> int
  | O230 : int -> int
  | O231 : int -> int
  | O232 : int -> int
  | O233 : int -> int
  | O234 : int -> int
  | O235 : int -> int
  | O236 : int -> int
  | O237 : int -> int
  | O238 : int -> int
  | O239 : int -> int
  | O240 : int -> int
  | O241 : int -> int
  | O242 : int -> int
  | O243 : int -> int
|}]

type duplicate = effect
  | O : int
  | O : float
[%%expect {|
Lines 1-3, characters 0-13:
1 | type duplicate = effect
2 |   | O : int
3 |   | O : float
Error: Two operations are named O
|}]

(* Variance *)

type +'a t = effect
  Foo : 'a -> int
[%%expect {|
type 'a t = effect Foo : 'a -> int
|}]

type 'a co = effect
  Foo : 'a -> int

type +'a t = 'a co
[%%expect {|
type 'a co = effect Foo : 'a -> int
type 'a t = 'a co
|}]

type +'a t = (int, 'a co) operation
[%%expect {|
type 'a t = (int, 'a co) operation
|}]

type -'a t = effect
  Foo : ('a -> int) -> int
[%%expect {|
type 'a t = effect Foo : ('a -> int) -> int
|}]

type 'a con = effect
  Foo : ('a -> int) -> int

type -'a t = 'a con
[%%expect {|
type 'a con = effect Foo : ('a -> int) -> int
type 'a t = 'a con
|}]

type -'a t = (int, 'a con) operation
[%%expect {|
type 'a t = (int, 'a con) operation
|}]

type +'a t = effect
  Foo : 'a -> 'a
[%%expect {|
Lines 1-2, characters 0-16:
1 | type +'a t = effect
2 |   Foo : 'a -> 'a
Error: In this definition, expected parameter variances are not satisfied.
       The 1st type parameter was expected to be covariant,
       but it is injective invariant.
|}]

type -'a t = effect
  Foo : 'a -> 'a
[%%expect {|
Lines 1-2, characters 0-16:
1 | type -'a t = effect
2 |   Foo : 'a -> 'a
Error: In this definition, expected parameter variances are not satisfied.
       The 1st type parameter was expected to be contravariant,
       but it is injective invariant.
|}]

type 'a inv = effect
  Foo : 'a -> 'a

type +'a t = 'a inv
[%%expect {|
type 'a inv = effect Foo : 'a -> 'a
Line 4, characters 0-19:
4 | type +'a t = 'a inv
    ^^^^^^^^^^^^^^^^^^^
Error: In this definition, expected parameter variances are not satisfied.
       The 1st type parameter was expected to be covariant,
       but it is injective invariant.
|}]

type +'a t = (int, 'a inv) operation
[%%expect {|
Line 1, characters 0-36:
1 | type +'a t = (int, 'a inv) operation
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this definition, expected parameter variances are not satisfied.
       The 1st type parameter was expected to be covariant,
       but it is injective invariant.
|}]

type -'a t = 'a inv
[%%expect {|
Line 1, characters 0-19:
1 | type -'a t = 'a inv
    ^^^^^^^^^^^^^^^^^^^
Error: In this definition, expected parameter variances are not satisfied.
       The 1st type parameter was expected to be contravariant,
       but it is injective invariant.
|}]

type -'a t = (int, 'a inv) operation
[%%expect {|
Line 1, characters 0-36:
1 | type -'a t = (int, 'a inv) operation
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this definition, expected parameter variances are not satisfied.
       The 1st type parameter was expected to be contravariant,
       but it is injective invariant.
|}]

type 'a reader = effect
  | Read : 'a

type +'a t = ('a, int reader) operation
[%%expect {|
type 'a reader = effect Read : 'a
Line 4, characters 0-39:
4 | type +'a t = ('a, int reader) operation
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this definition, expected parameter variances are not satisfied.
       The 1st type parameter was expected to be covariant,
       but it is injective invariant.
|}]

type -'a t = ('a, int reader) operation
[%%expect {|
Line 1, characters 0-39:
1 | type -'a t = ('a, int reader) operation
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this definition, expected parameter variances are not satisfied.
       The 1st type parameter was expected to be contravariant,
       but it is injective invariant.
|}]

(* Immediacy *)

type 'a reader = effect
  | Read : 'a
[@@immediate]
[%%expect {|
type 'a reader = effect Read : 'a [@@immediate]
|}]

type 'a state = effect
  | Get : 'a
  | Set : 'a -> unit
[@@immediate]
[%%expect {|
Lines 1-4, characters 0-13:
1 | type 'a state = effect
2 |   | Get : 'a
3 |   | Set : 'a -> unit
4 | [@@immediate]
Error: Types marked with the immediate attribute must be non-pointer types
       like "int" or "bool".
|}]

type 'a t = 'a reader
[@@immediate]
[%%expect {|
type 'a t = 'a reader [@@immediate]
|}]

(* Fix this after immediacy calculation is improved by PR??? *)
type 'a t = (int, 'a reader) operation
[@@immediate]
[%%expect {|
Lines 1-2, characters 0-13:
1 | type 'a t = (int, 'a reader) operation
2 | [@@immediate]
Error: Types marked with the immediate attribute must be non-pointer types
       like "int" or "bool".
|}]

(* Inclusion *) 

module M : sig

  type 'a t

end = struct

  type 'a t = effect
   | Foo : 'a

end
[%%expect {|
module M : sig type 'a t end
|}]

module M : sig

  type 'a t = effect
   | Foo : 'a

end = struct

  type 'a t = effect
   | Foo : 'a

end
[%%expect {|
module M : sig type 'a t = effect Foo : 'a end
|}]

module M : sig

  type 'a t =
   | Foo

end = struct

  type 'a t = effect
   | Foo : 'a

end
[%%expect {|
Lines 6-11, characters 6-3:
 6 | ......struct
 7 |
 8 |   type 'a t = effect
 9 |    | Foo : 'a
10 |
11 | end
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = effect Foo : 'a end
       is not included in
         sig type 'a t = Foo end
       Type declarations do not match:
         type 'a t = effect Foo : 'a
       is not included in
         type 'a t = Foo
       The first is an effect, but the second is a variant.
|}]

module M : sig

  type 'a t =
   { foo : 'a }

end = struct

  type 'a t = effect
   | Foo : 'a

end
[%%expect {|
Lines 6-11, characters 6-3:
 6 | ......struct
 7 |
 8 |   type 'a t = effect
 9 |    | Foo : 'a
10 |
11 | end
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = effect Foo : 'a end
       is not included in
         sig type 'a t = { foo : 'a; } end
       Type declarations do not match:
         type 'a t = effect Foo : 'a
       is not included in
         type 'a t = { foo : 'a; }
       The first is an effect, but the second is a record.
|}]

module M : sig

  type 'a t = ..

end = struct

  type 'a t = effect
   | Foo : 'a

end
[%%expect {|
Lines 5-10, characters 6-3:
 5 | ......struct
 6 |
 7 |   type 'a t = effect
 8 |    | Foo : 'a
 9 |
10 | end
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = effect Foo : 'a end
       is not included in
         sig type 'a t = .. end
       Type declarations do not match:
         type 'a t = effect Foo : 'a
       is not included in
         type 'a t = ..
       The first is an effect, but the second is an extensible variant.
|}]

module M : sig

  type 'a t = effect
   | Foz : 'a

end = struct

  type 'a t = effect
   | Foo : 'a

end
[%%expect {|
Lines 6-11, characters 6-3:
 6 | ......struct
 7 |
 8 |   type 'a t = effect
 9 |    | Foo : 'a
10 |
11 | end
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = effect Foo : 'a end
       is not included in
         sig type 'a t = effect Foz : 'a end
       Type declarations do not match:
         type 'a t = effect Foo : 'a
       is not included in
         type 'a t = effect Foz : 'a
       Operations have different names, Foo and Foz.
|}]

module M : sig

  type 'a t = effect
   | Foo : 'a
   | Bar : 'a

end = struct

  type 'a t = effect
   | Foo : 'a

end
[%%expect {|
Lines 7-12, characters 6-3:
 7 | ......struct
 8 |
 9 |   type 'a t = effect
10 |    | Foo : 'a
11 |
12 | end
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = effect Foo : 'a end
       is not included in
         sig type 'a t = effect Foo : 'a | Bar : 'a end
       Type declarations do not match:
         type 'a t = effect Foo : 'a
       is not included in
         type 'a t = effect Foo : 'a | Bar : 'a
       An operatoin, Bar, is missing in the first declaration.
|}]

module M : sig

  type 'a t = effect
   | Bar : 'a
   | Foo : 'a

end = struct

  type 'a t = effect
   | Foo : 'a
   | Bar : 'a

end
[%%expect {|
Lines 7-13, characters 6-3:
 7 | ......struct
 8 |
 9 |   type 'a t = effect
10 |    | Foo : 'a
11 |    | Bar : 'a
12 |
13 | end
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = effect Foo : 'a | Bar : 'a end
       is not included in
         sig type 'a t = effect Bar : 'a | Foo : 'a end
       Type declarations do not match:
         type 'a t = effect Foo : 'a | Bar : 'a
       is not included in
         type 'a t = effect Bar : 'a | Foo : 'a
       Operations Bar and Foo have been swapped.
|}]

module M : sig

  type 'a t = effect
   | Foo : 'a

end = struct

  type 'a t = effect
   | Foo : 'a
   | Bar : 'a

end
[%%expect {|
Lines 6-12, characters 6-3:
 6 | ......struct
 7 |
 8 |   type 'a t = effect
 9 |    | Foo : 'a
10 |    | Bar : 'a
11 |
12 | end
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = effect Foo : 'a | Bar : 'a end
       is not included in
         sig type 'a t = effect Foo : 'a end
       Type declarations do not match:
         type 'a t = effect Foo : 'a | Bar : 'a
       is not included in
         type 'a t = effect Foo : 'a
       An extra operation, Bar, is provided in the first declaration.
|}]

module M : sig

  type t = effect
   | Foo : int * float -> string

end = struct

  type t = effect
   | Foo : int -> string

end
[%%expect {|
Lines 6-11, characters 6-3:
 6 | ......struct
 7 |
 8 |   type t = effect
 9 |    | Foo : int -> string
10 |
11 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = effect Foo : int -> string end
       is not included in
         sig type t = effect Foo : int * float -> string end
       Type declarations do not match:
         type t = effect Foo : int -> string
       is not included in
         type t = effect Foo : int * float -> string
       Operations do not match:
         Foo : int -> string
       is not the same as:
         Foo : int * float -> string
       They have different arities.
|}]

module M : sig

  type t = effect
   | Foo : int * float -> string

end = struct

  type t = effect
   | Foo : (int * float) -> string

end
[%%expect {|
Lines 6-11, characters 6-3:
 6 | ......struct
 7 |
 8 |   type t = effect
 9 |    | Foo : (int * float) -> string
10 |
11 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = effect Foo : (int * float) -> string end
       is not included in
         sig type t = effect Foo : int * float -> string end
       Type declarations do not match:
         type t = effect Foo : (int * float) -> string
       is not included in
         type t = effect Foo : int * float -> string
       Operations do not match:
         Foo : (int * float) -> string
       is not the same as:
         Foo : int * float -> string
       They have different arities.
|}]

module M : sig

  type t = effect
   | Foo : int -> string

end = struct

  type t = effect
   | Foo : { bar : int } -> string

end
[%%expect {|
Lines 6-11, characters 6-3:
 6 | ......struct
 7 |
 8 |   type t = effect
 9 |    | Foo : { bar : int } -> string
10 |
11 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = effect Foo : { bar : int; } -> string end
       is not included in
         sig type t = effect Foo : int -> string end
       Type declarations do not match:
         type t = effect Foo : { bar : int; } -> string
       is not included in
         type t = effect Foo : int -> string
       Operations do not match:
         Foo : { bar : int; } -> string
       is not the same as:
         Foo : int -> string
       The first uses inline records and the second doesn't.
|}]

module M : sig

  type t = effect
   | Foo : { foo : int } -> string

end = struct

  type t = effect
   | Foo : { bar : int } -> string

end
[%%expect {|
Lines 6-11, characters 6-3:
 6 | ......struct
 7 |
 8 |   type t = effect
 9 |    | Foo : { bar : int } -> string
10 |
11 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = effect Foo : { bar : int; } -> string end
       is not included in
         sig type t = effect Foo : { foo : int; } -> string end
       Type declarations do not match:
         type t = effect Foo : { bar : int; } -> string
       is not included in
         type t = effect Foo : { foo : int; } -> string
       Operations do not match:
         Foo : { bar : int; } -> string
       is not the same as:
         Foo : { foo : int; } -> string
       Fields have different names, "bar" and "foo".
|}]

module M : sig

  type t = effect
   | Foo : { foo : int; bar : float } -> string

end = struct

  type t = effect
   | Foo : { foo : int } -> string

end
[%%expect {|
Lines 6-11, characters 6-3:
 6 | ......struct
 7 |
 8 |   type t = effect
 9 |    | Foo : { foo : int } -> string
10 |
11 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = effect Foo : { foo : int; } -> string end
       is not included in
         sig type t = effect Foo : { foo : int; bar : float; } -> string end
       Type declarations do not match:
         type t = effect Foo : { foo : int; } -> string
       is not included in
         type t = effect Foo : { foo : int; bar : float; } -> string
       Operations do not match:
         Foo : { foo : int; } -> string
       is not the same as:
         Foo : { foo : int; bar : float; } -> string
       A field, "bar", is missing in the first declaration.
|}]

module M : sig

  type t = effect
   | Foo : { bar : float; foo : int } -> string

end = struct

  type t = effect
   | Foo : { foo : int; bar : float } -> string

end
[%%expect {|
Lines 6-11, characters 6-3:
 6 | ......struct
 7 |
 8 |   type t = effect
 9 |    | Foo : { foo : int; bar : float } -> string
10 |
11 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = effect Foo : { foo : int; bar : float; } -> string end
       is not included in
         sig type t = effect Foo : { bar : float; foo : int; } -> string end
       Type declarations do not match:
         type t = effect Foo : { foo : int; bar : float; } -> string
       is not included in
         type t = effect Foo : { bar : float; foo : int; } -> string
       Operations do not match:
         Foo : { foo : int; bar : float; } -> string
       is not the same as:
         Foo : { bar : float; foo : int; } -> string
       Field "bar" has been moved from position 1 to 2.
|}]

module M : sig

  type t = effect
   | Foo : { foo : int } -> string

end = struct

  type t = effect
   | Foo : { foo : int; bar : float } -> string

end
[%%expect {|
Lines 6-11, characters 6-3:
 6 | ......struct
 7 |
 8 |   type t = effect
 9 |    | Foo : { foo : int; bar : float } -> string
10 |
11 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = effect Foo : { foo : int; bar : float; } -> string end
       is not included in
         sig type t = effect Foo : { foo : int; } -> string end
       Type declarations do not match:
         type t = effect Foo : { foo : int; bar : float; } -> string
       is not included in
         type t = effect Foo : { foo : int; } -> string
       Operations do not match:
         Foo : { foo : int; bar : float; } -> string
       is not the same as:
         Foo : { foo : int; } -> string
       An extra field, "bar", is provided in the first declaration.
|}]

module M : sig

  type t = effect
   | Foo : int

end = struct

  type t = effect
   | Foo : string

end
[%%expect {|
Lines 6-11, characters 6-3:
 6 | ......struct
 7 |
 8 |   type t = effect
 9 |    | Foo : string
10 |
11 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = effect Foo : string end
       is not included in
         sig type t = effect Foo : int end
       Type declarations do not match:
         type t = effect Foo : string
       is not included in
         type t = effect Foo : int
       Operations do not match:
         Foo : string
       is not the same as:
         Foo : int
       The type "string" is not equal to the type "int"
|}]

module M : sig

  type t = effect
   | Foo : int -> int

end = struct

  type t = effect
   | Foo : string -> int

end
[%%expect {|
Lines 6-11, characters 6-3:
 6 | ......struct
 7 |
 8 |   type t = effect
 9 |    | Foo : string -> int
10 |
11 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = effect Foo : string -> int end
       is not included in
         sig type t = effect Foo : int -> int end
       Type declarations do not match:
         type t = effect Foo : string -> int
       is not included in
         type t = effect Foo : int -> int
       Operations do not match:
         Foo : string -> int
       is not the same as:
         Foo : int -> int
       The type "string" is not equal to the type "int"
|}]

module M : sig

  type t = effect
   | Foo : { foo : int } -> int

end = struct

  type t = effect
   | Foo : { foo : string } -> int

end
[%%expect {|
Lines 6-11, characters 6-3:
 6 | ......struct
 7 |
 8 |   type t = effect
 9 |    | Foo : { foo : string } -> int
10 |
11 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = effect Foo : { foo : string; } -> int end
       is not included in
         sig type t = effect Foo : { foo : int; } -> int end
       Type declarations do not match:
         type t = effect Foo : { foo : string; } -> int
       is not included in
         type t = effect Foo : { foo : int; } -> int
       Operations do not match:
         Foo : { foo : string; } -> int
       is not the same as:
         Foo : { foo : int; } -> int
       Fields do not match:
         "foo : string;"
       is not the same as:
         "foo : int;"
       The type "string" is not equal to the type "int"
|}]

module M : sig

  type ('b, 'a) t = effect
   | Foo : 'b -> 'a

end = struct

  type ('a, 'b) t = effect
   | Foo : 'a -> 'b

end
[%%expect {|
module M : sig type ('b, 'a) t = effect Foo : 'b -> 'a end
|}]

module M : sig

  type ('a, 'b) t = effect
   | Foo : 'b -> 'a

end = struct

  type ('a, 'b) t = effect
   | Foo : 'a -> 'b

end
[%%expect {|
Lines 6-11, characters 6-3:
 6 | ......struct
 7 |
 8 |   type ('a, 'b) t = effect
 9 |    | Foo : 'a -> 'b
10 |
11 | end
Error: Signature mismatch:
       Modules do not match:
         sig type ('a, 'b) t = effect Foo : 'a -> 'b end
       is not included in
         sig type ('a, 'b) t = effect Foo : 'b -> 'a end
       Type declarations do not match:
         type ('a, 'b) t = effect Foo : 'a -> 'b
       is not included in
         type ('a, 'b) t = effect Foo : 'b -> 'a
       Operations do not match:
         Foo : 'a -> 'b
       is not the same as:
         Foo : 'b -> 'a
       The type "'b" is not equal to the type "'a"
|}]

module M : sig

  type 'a t = effect
   | Foo : 'b -> 'a

end = struct

  type 'b t = effect
   | Foo : 'a -> 'b

end
[%%expect {|
module M : sig type 'a t = effect Foo : 'b -> 'a end
|}]

module M : sig

  type 'a t = effect
   | Foo : 'b -> 'a

end = struct

  type 'a t = effect
   | Foo : 'a -> 'b

end
[%%expect {|
Lines 6-11, characters 6-3:
 6 | ......struct
 7 |
 8 |   type 'a t = effect
 9 |    | Foo : 'a -> 'b
10 |
11 | end
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = effect Foo : 'a -> 'b end
       is not included in
         sig type 'a t = effect Foo : 'b -> 'a end
       Type declarations do not match:
         type 'a t = effect Foo : 'a -> 'b
       is not included in
         type 'a t = effect Foo : 'b -> 'a
       Operations do not match:
         Foo : 'a -> 'b
       is not the same as:
         Foo : 'b -> 'a
       The type "'b" is not equal to the type "'a"
|}]

module M : sig

  type 'a t = effect
   | Foo : 'a * 'b * 'c -> 'c

end = struct

  type 'a t = effect
   | Foo : 'a * 'b * 'c -> 'c

end
[%%expect {|
module M : sig type 'a t = effect Foo : 'a * 'b * 'c -> 'c end
|}]

module M : sig

  type 'a t = effect
   | Foo : 'a * 'b * 'c -> 'a

end = struct

  type 'a t = effect
   | Foo : 'a * 'b * 'c -> 'c

end
[%%expect {|
Lines 6-11, characters 6-3:
 6 | ......struct
 7 |
 8 |   type 'a t = effect
 9 |    | Foo : 'a * 'b * 'c -> 'c
10 |
11 | end
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = effect Foo : 'a * 'b * 'c -> 'c end
       is not included in
         sig type 'a t = effect Foo : 'a * 'b * 'c -> 'a end
       Type declarations do not match:
         type 'a t = effect Foo : 'a * 'b * 'c -> 'c
       is not included in
         type 'a t = effect Foo : 'a * 'b * 'c -> 'a
       Operations do not match:
         Foo : 'a * 'b * 'c -> 'c
       is not the same as:
         Foo : 'a * 'b * 'c -> 'a
       The type "'c" is not equal to the type "'a"
|}]

module M : sig

  type 'a t = effect
   | Foo : 'a * 'b * 'c -> 'b

end = struct

  type 'a t = effect
   | Foo : 'a * 'b * 'c -> 'c

end
[%%expect {|
Lines 6-11, characters 6-3:
 6 | ......struct
 7 |
 8 |   type 'a t = effect
 9 |    | Foo : 'a * 'b * 'c -> 'c
10 |
11 | end
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = effect Foo : 'a * 'b * 'c -> 'c end
       is not included in
         sig type 'a t = effect Foo : 'a * 'b * 'c -> 'b end
       Type declarations do not match:
         type 'a t = effect Foo : 'a * 'b * 'c -> 'c
       is not included in
         type 'a t = effect Foo : 'a * 'b * 'c -> 'b
       Operations do not match:
         Foo : 'a * 'b * 'c -> 'c
       is not the same as:
         Foo : 'a * 'b * 'c -> 'b
       The type "'b" is not equal to the type "'c"
|}]

module M : sig

  type 'a t = effect
   | Foo : 'a * 'b * 'c -> 'c

end = struct

  type 'a t = effect
   | Foo : 'a * 'b * 'c -> 'b

end
[%%expect {|
Lines 6-11, characters 6-3:
 6 | ......struct
 7 |
 8 |   type 'a t = effect
 9 |    | Foo : 'a * 'b * 'c -> 'b
10 |
11 | end
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = effect Foo : 'a * 'b * 'c -> 'b end
       is not included in
         sig type 'a t = effect Foo : 'a * 'b * 'c -> 'c end
       Type declarations do not match:
         type 'a t = effect Foo : 'a * 'b * 'c -> 'b
       is not included in
         type 'a t = effect Foo : 'a * 'b * 'c -> 'c
       Operations do not match:
         Foo : 'a * 'b * 'c -> 'b
       is not the same as:
         Foo : 'a * 'b * 'c -> 'c
       The type "'b" is not equal to the type "'b0"
|}]

module M : sig

  type 'a t = effect
   | Foo : 'a * 'b * 'c -> 'c

end = struct

  type 'a t = effect
   | Foo : 'a * 'b * 'c -> 'a

end
[%%expect {|
Lines 6-11, characters 6-3:
 6 | ......struct
 7 |
 8 |   type 'a t = effect
 9 |    | Foo : 'a * 'b * 'c -> 'a
10 |
11 | end
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = effect Foo : 'a * 'b * 'c -> 'a end
       is not included in
         sig type 'a t = effect Foo : 'a * 'b * 'c -> 'c end
       Type declarations do not match:
         type 'a t = effect Foo : 'a * 'b * 'c -> 'a
       is not included in
         type 'a t = effect Foo : 'a * 'b * 'c -> 'c
       Operations do not match:
         Foo : 'a * 'b * 'c -> 'a
       is not the same as:
         Foo : 'a * 'b * 'c -> 'c
       The type "'a" is not equal to the type "'c"
|}]
