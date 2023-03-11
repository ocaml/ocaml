/* TEST
   ocamlyacc_flags = " -q --strict "
*/
%token <unit> SIMPLE
%type <unit> silly
%start silly
%%
silly : SIMPLE {
            (* {A|  *)
            (* {a| *) } } }  |a} *)
            (* {%%A.ba| *) } } }  |} *)
    $1
    }
;
