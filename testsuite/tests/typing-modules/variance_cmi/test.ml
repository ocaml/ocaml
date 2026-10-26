(* TEST
 readonly_files = "q.mli phantom.mli client_bad.ml client_ok.ml";
 setup-ocamlc.byte-build-env;

 (* Compile abstract and manifest type declarations separately so clients
    consume the variance metadata from their CMIs. *)
 module = "q.mli";
 ocamlc.byte;
 module = "phantom.mli";
 ocamlc.byte;

 (* An abstract bivariant declaration can have an injective implementation.
    Its CMI must retain that possibility when used by a downstream functor. *)
 module = "client_bad.ml";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;

 (* A manifest phantom alias must remain distinguishable after CMI loading. *)
 module = "client_ok.ml";
 ocamlc_byte_exit_status = "0";
 ocamlc.byte;
*)
