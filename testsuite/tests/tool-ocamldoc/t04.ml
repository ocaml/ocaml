(* TEST
 plugins = "odoc_test.ml";
 flags = "-I ${ocamlsrcdir}/ocamldoc";
 ocamldoc;
*)

(** Testing display of inline record.

   @test_types_display
 *)


module A = struct
  type a = A of {lbl:int}

end

module type E = sig
  exception E of {lbl:int}

end


module E_bis= struct
  exception E of {lbl:int}
end
