
(****************************************)

value interf_p =
  Grammar.Entry.of_parser Pcaml.gram "interf" interf_0
;

value implem_p =
  Grammar.Entry.of_parser Pcaml.gram "implem" implem_0
;

value top_phrase_p =
  Grammar.Entry.of_parser Pcaml.gram "top_phrase" top_phrase_0
;

value use_file_p =
  Grammar.Entry.of_parser Pcaml.gram "use_file" use_file_0
;

EXTEND
  interf:
    [ [ x = interf_p -> x ] ]
  ;
  implem:
    [ [ x = implem_p -> x ] ]
  ;
  top_phrase:
    [ [ x = top_phrase_p -> x ] ]
  ;
  use_file:
    [ [ x = use_file_p -> x ] ]
  ;
END;
