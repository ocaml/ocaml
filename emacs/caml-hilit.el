; Highlighting patterns for hilit19 under caml-mode

; defined also in caml.el
(defvar caml-quote-char "'"
  "*Quote for character constants. \"'\" for Objective Caml, \"`\" for Caml-Light.")

(defconst caml-mode-patterns
  (list
;comments
   '("\\(^\\|[^\"]\\)\\((\\*[^*]*\\*+\\([^)*][^*]*\\*+\\)*)\\)"
     2 comment)
;string
   (list 'hilit-string-find (string-to-char caml-quote-char) 'string)
   (list (concat caml-quote-char "\\(\\\\\\([ntbr" caml-quote-char "\\]\\|"
		 "[0-9][0-9][0-9]\\)\\|.\\)" caml-quote-char)
	 nil
	 'string)
;labels
   '("[?]?\\<[A-Za-z][A-Za-z0-9_\']*:" nil brown)
   '("[?]?\\<:[A-Za-z][A-Za-z0-9_\']*\\>" nil brown)
;modules
   '("\\<\\(assert\\|open\\|include\\)\\>" nil brown)
   '("\\<[A-Z][A-Za-z0-9_\']*\\>" nil MidnightBlue)
   '("`[A-Za-z][A-Za-z0-9_\']*\\>" nil MidnightBlue)
;definition
   (list (concat
	  "\\<\\(a\\(nd\\|s\\)\\|c\\(onstraint\\|lass\\)"
	  "\\|ex\\(ception\\|ternal\\)\\|fun\\(ct\\(ion\\|or\\)\\)?"
	  "\\|in\\(herit\\)?\\|let\\|m\\(ethod\\|utable\\|odule\\)"
	  "\\|of\\|p\\(arser\\|rivate\\)\\|rec\\|type"
	  "\\|v\\(al\\(ue\\)?\\|irtual\\)\\)\\>")
	 nil 'ForestGreen)
;blocking
   '("\\(\\<\\|:\\)\\(object\\|struct\\|sig\\|begin\\|end\\)\\>"
     2 include)
;control
   (list (concat
	  "\\<\\(do\\(ne\\|wnto\\)?\\|else\\|for\\|if"
	  "\\|lazy\\|match\\|new\\|or\\|t\\(hen\\|o\\|ry\\)"
	  "\\|w\\(h\\(en\\|ile\\)\\|ith\\)\\)\\>"
	  "\\|\|\\|->\\|&\\|#")
	 nil 'keyword)
   '(";" nil struct))
  "Hilit19 patterns used for Caml mode")

(hilit-set-mode-patterns 'caml-mode caml-mode-patterns)
(hilit-set-mode-patterns
 'inferior-caml-mode
 (append
  (list
;inferior
   '("^[#-]"	nil	firebrick)
   '("`[A-Za-z][A-Za-z0-9_\']*\\>" nil MidnightBlue)
   '("[? \t]:[A-Za-z][A-Za-z0-9_\']*\\>" nil brown))
  caml-mode-patterns))

(provide 'caml-hilit)
