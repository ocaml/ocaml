(* TEST
   expect;
*)

module Annot = struct
  type 'a t = 'a constraint
    'a = < per_field : 'a0
         ; per_group : 'a1
         ; per_entry : 'a7
         >

  type 'a per_field = 'b constraint 'a = < per_field : 'b; .. > t
  type 'a per_group = 'b constraint 'a = < per_group : 'b; .. > t
  type 'a per_entry = 'b constraint 'a = < per_entry : 'b; .. > t
end

module rec Entry : sig
  type ('a, 's) t' =
    | Field : 'a Field.t -> ('a Annot.t, 'a Annot.per_field) t'
    | Group : 'a Group.t -> ('a Annot.t, 'a Annot.per_group) t'

  type 'a t =
      E : { t : ('a, 's) t'
          ; per_entry : 'a Annot.per_entry
          ; specific : 's
          } -> 'a t
end = Entry
and Field : sig
  type _ t =
    | F : { i : int
          } -> _ t
end = Field
and Group : sig
  type 'a t =
    { i : int
    ; optional : bool
    }
end = Group

module type Annotator = sig
  type pre
  type post
  type contents

  val annotate : pre -> contents -> post
end

type ('pre, 'post, 'contents) t = (module Annotator with type pre = 'pre and type post = 'post and type contents = 'contents)

module Presence = struct
  type t =
    | Always
    | Sometimes
end

[%%expect{|
module Annot :
  sig
    type 'a t = 'a
      constraint 'a = < per_entry : 'a7; per_field : 'a0; per_group : 'a1 >
    type 'a per_field = 'b
      constraint 'a = < per_entry : 'c; per_field : 'b; per_group : 'd > t
    type 'a per_group = 'b
      constraint 'a = < per_entry : 'c; per_field : 'd; per_group : 'b > t
    type 'a per_entry = 'b
      constraint 'a = < per_entry : 'b; per_field : 'c; per_group : 'd > t
  end
module rec Entry :
  sig
    type ('a, 's) t' =
        Field :
          < per_entry : 'b; per_field : 'c; per_group : 'd > Annot.t Field.t ->
          (< per_entry : 'b; per_field : 'c; per_group : 'd > Annot.t Annot.t,
           < per_entry : 'b; per_field : 'c; per_group : 'd > Annot.t
           Annot.per_field)
          t'
      | Group :
          < per_entry : 'e; per_field : 'f; per_group : 'g > Annot.t Group.t ->
          (< per_entry : 'e; per_field : 'f; per_group : 'g > Annot.t Annot.t,
           < per_entry : 'e; per_field : 'f; per_group : 'g > Annot.t
           Annot.per_group)
          t'
    type 'a t =
        E : {
          t :
            (< per_entry : 'b; per_field : 'c; per_group : 'd > Annot.t, 's)
            t';
          per_entry :
            < per_entry : 'b; per_field : 'c; per_group : 'd > Annot.t
            Annot.per_entry;
          specific : 's;
        } -> < per_entry : 'b; per_field : 'c; per_group : 'd > Annot.t t
  end
and Field : sig type _ t = F : { i : int; } -> 'a t end
and Group : sig type 'a t = { i : int; optional : bool; } end
module type Annotator =
  sig
    type pre
    type post
    type contents
    val annotate : pre -> contents -> post
  end
type ('pre, 'post, 'contents) t =
    (module Annotator with type contents = 'contents and type post = 'post and type pre = 'pre)
module Presence : sig type t = Always | Sometimes end
|}]

let rec annotate_presence
  (type pre pos)
  (type a)
  (annotator : (pre, pos, Presence.t) t)
  (e : a Entry.t)
  parent_presence
  =
  match e with
  | E e ->
    let this_presence =
      match e.t with
      | Field _ -> parent_presence
      | Group g ->
        if g.optional
        then Presence.Sometimes
        else parent_presence
    in
    Entry.E e

[%%expect{|
Line 18, characters 12-13:
18 |     Entry.E e
                 ^
Error: The value "e" has type "($s, $2, $1, $0) Entry.t.E"
       but an expression was expected of type "($s, 'a, 'b, 'c) Entry.t.E"
       The type constructor "$2" would escape its scope
       Hint: "$s" is an existential type bound by the constructor "E".
       Hint: "$0", "$1" and "$2" are type variables introduced in the equation
         "a" = "< per_entry : $0; per_field : $1; per_group : $2 >"
|}]
