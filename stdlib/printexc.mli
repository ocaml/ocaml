(* A catch-all exception handler *)

val catch: ('a -> 'b) -> 'a -> 'b
        (* [Printexc.catch fn x] applies [fn] to [x] and returns the result.
           If the evaluation of [fn x] raises any exception, the
           name of the exception is printed on standard error output,
           and the programs aborts with exit code 2.
           Typical use is [Printexc.catch main ()], where [main], with type
           [unit->unit], is the entry point of a standalone program, to catch
           and print stray exceptions. *)

val print: ('a -> 'b) -> 'a -> 'b
        (* Same as [catch], but re-raise the stray exception after
           printing it, instead of aborting the program. *)
