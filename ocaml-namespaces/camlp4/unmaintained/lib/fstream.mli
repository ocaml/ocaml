(* camlp4r *)


(* Module [Fstream]: functional streams *)

(* This module implement functional streams.
   To be used with syntax [pa_fstream.cmo]. The syntax is:
-     stream: [fstream [: ... :]]
-     parser: [parser [ [: ... :] -> ... | ... ]]

   Functional parsers are of type: [Fstream.t 'a -> option ('a * Fstream.t 'a)]

   They have limited backtrack, i.e if a rule fails, the next rule is tested
   with the initial stream; limited because when in case of a rule with two
   consecutive symbols [a] and [b], if [b] fails, the rule fails: there is
   no try with the next rule of [a].
*)

type t 'a = 'x;
    (* The type of 'a functional streams *)
value from : (int -> option 'a) -> t 'a;
    (* [Fstream.from f] returns a stream built from the function [f].
       To create a new stream element, the function [f] is called with
       the current stream count. The user function [f] must return either
       [Some <value>] for a value or [None] to specify the end of the
       stream. *)

value of_list : list 'a -> t 'a;
    (* Return the stream holding the elements of the list in the same
       order. *)
value of_string : string -> t char;
    (* Return the stream of the characters of the string parameter. *)
value of_channel : in_channel -> t char;
    (* Return the stream of the characters read from the input channel. *)

value iter : ('a -> unit) -> t 'a -> unit;
    (* [Fstream.iter f s] scans the whole stream s, applying function [f]
       in turn to each stream element encountered. *)

value next : t 'a -> option ('a * t 'a);
    (* Return [Some (a, s)] where [a] is the first element of the stream
       and [s] the remaining stream, or [None] if the stream is empty. *)
value empty : t 'a -> option (unit * t 'a);
    (* Return [Some ((), s)] if the stream is empty where [s] is itself,
       else [None] *)
value count : t 'a -> int;
    (* Return the current count of the stream elements, i.e. the number
       of the stream elements discarded. *)
value count_unfrozen : t 'a -> int;
    (* Return the number of unfrozen elements in the beginning of the
       stream; useful to determine the position of a parsing error (longuest
       path). *)

(*--*)

value nil : t 'a;
type data 'a = 'x;
value cons : 'a -> t 'a -> data 'a;
value app : t 'a -> t 'a -> data 'a;
value flazy : (unit -> data 'a) -> t 'a;
