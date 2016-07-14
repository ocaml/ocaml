(**
  This test focuses on the printing of documentation for inline record
  within the latex generator.
*)    


(** A simple record type for reference *)
type r = {lbl: int (** Field documentation *);
          more:int list (** More documentation *) }


(** A sum type with an inline record *)
type t = C of {lbl: int (** Field documentation *); 
               more:int list (** More documentation *) }
