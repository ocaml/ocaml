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
   '("\\<open\\>" nil brown)
   '("\\<[A-Z][A-Za-z0-9_\']*\\>" nil MidnightBlue)
   '("`[A-Za-z][A-Za-z0-9_\']*\\>" nil MidnightBlue)
;definition
   (list (concat
	  "\\<\\(let\\|rec\\|in\\|type\\|of\\|and"
	  "\\|exception\\|val\\|and\\|function\\|fun"
	  "\\|parser\\|mutable\\|module\\|inherit"
	  "\\|external\\|method\\|virtual\\|private"
	  "\\|constraint\\|as\\|closed\\)\\>")
	 nil 'ForestGreen)
;blocking
   '("\\(\\<\\|:\\)\\(class\\|struct\\|sig\\|begin\\|end\\)\\>"
     2 include)
;control
   (list (concat
	  "\\<\\(if\\|then\\|else\\|match\\|when"
	  "\\|with\\|try\\|for\\|do\\|while\\|done"
	  "\\|downto\\|to\\|or\\|new\\)\\>"
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
