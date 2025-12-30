(*
   Check whether a rule may fail to match on some input
*)

open Printf
open Exhausteve

(* Translate to the regexp type used by Exhausteve *)
let rec translate_regexp (re : Syntax.regular_expression) : Regexp.t =
  match re with
  | Epsilon -> Epsilon
  | Characters cset ->
      (* assume the ints in Cset are [0,255] *)
      Char (Cset.to_list cset
            |> List.map Char.chr
            |> Char_class.of_list)
  | Eof -> End_of_input
  | Sequence (a, b) -> Seq (translate_regexp a, translate_regexp b)
  | Alternative (a, b) -> Alt (translate_regexp a, translate_regexp b)
  | Repetition a -> Repeat (translate_regexp a)
  | Bind (a, _ident) -> translate_regexp a

(* Create one regular expression from all the cases in the rule *)
let build_disjunction_regexp (e : _ Syntax.entry) : Regexp.t =
  let regexps =
    List.map (fun (re, _loc) -> translate_regexp re) e.clauses in
  match List.rev regexps with
  | [] -> Char Char_class.empty
  | last :: regexps ->
      List.fold_right (fun re acc -> Regexp.Alt (re, acc)) regexps last

(* TODO: obtain a good location for the rule and report it here *)
let warning (loc : Syntax.location) msg =
  eprintf "File \"%s\", line 1, character 0:\n\
           Warning: %s\n\
          "
    loc.loc_file msg

(* This is a failed attempt to extract the location of the first pattern.
   Unfortunately, it's the location of the action ('{') *)
let loc_of_entry fallback_loc (e : (_, Syntax.location) Syntax.entry) =
  match e.clauses with
  | [] -> fallback_loc
  | (_, loc) :: _ -> loc

let check_entrypoint loc (e : (string list, Syntax.location) Syntax.entry) =
  let regexp = build_disjunction_regexp e in
  let dfa = Check.compile Conf.Prefix regexp in
  match Check.is_exhaustive dfa with
  | Ok () -> ()
  | Error example ->
      let loc = loc_of_entry loc e in
      warning loc (sprintf "rule %s is not exhaustive.\n\
                            Here is an example of nonmatching input:\n\
                            %S"
                     e.name example)

let check loc entrypoints =
  List.iter (check_entrypoint loc) entrypoints
