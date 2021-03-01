(* TEST *)

(* We could not include the following characters the file name:

   - '$' : this character is interpreted specially by [ocamltest] (as it uses
     [Buffer.add_substitute] on the filenames).

   - '^' : this character causes problems under Windows if not properly
     quoted. In particular, flexlink needed to be adapted.
*)
