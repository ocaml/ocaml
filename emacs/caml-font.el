;; useful colors

(cond
 ((and (x-display-color-p)
       (not (memq 'font-lock-type-face (face-list))))
  ; make the necessary faces
  (make-face 'Firebrick)
  (set-face-foreground 'Firebrick "Firebrick")
  (make-face 'RosyBrown)
  (set-face-foreground 'RosyBrown "RosyBrown")
  (make-face 'Purple)
  (set-face-foreground 'Purple "Purple")
  (make-face 'MidnightBlue)
  (set-face-foreground 'MidnightBlue "MidnightBlue")
  (make-face 'DarkGoldenRod)
  (set-face-foreground 'DarkGoldenRod "DarkGoldenRod")
  (make-face 'DarkOliveGreen)
  (set-face-foreground 'DarkOliveGreen "DarkOliveGreen4")
  (make-face 'CadetBlue)
  (set-face-foreground 'CadetBlue "CadetBlue")
  ; assign them as standard faces
  (setq font-lock-comment-face 'Firebrick)
  (setq font-lock-string-face 'RosyBrown)
  (setq font-lock-keyword-face 'Purple)
  (setq font-lock-function-name-face 'MidnightBlue)
  (setq font-lock-variable-name-face 'DarkGoldenRod)
  (setq font-lock-type-face 'DarkOliveGreen)
  (setq font-lock-reference-face 'CadetBlue)))

; The same definition is in caml.el:
; we don't know in which order they will be loaded.
(defvar caml-quote-char "'"
  "*Quote for character constants. \"'\" for Objective Caml, \"`\" for Caml-Light.")

(defconst caml-font-lock-keywords
  (list
;comments
   '("\\(^\\|[^\"]\\)\\((\\*[^*]*\\*+\\([^)*][^*]*\\*+\\)*)\\)"
     2 font-lock-comment-face)
;character literals
   (cons (concat caml-quote-char "\\(\\\\\\([ntbr" caml-quote-char "\\]\\|"
		 "[0-9][0-9][0-9]\\)\\|.\\)" caml-quote-char
		 "\\|\"[^\"\\]*\\(\\\\\\(.\\|\n\\)[^\"\\]*\\)*\"")
	 'font-lock-string-face)
;labels (and open)
   '("\\([?]?\\<[A-Za-z][A-Za-z0-9_']*:\\)\\([^:=]\\|\\'\\|$\\)" 1
     font-lock-variable-name-face)
   '("\\<\\(open\\|include\\)\\>\\|[?]?\\<:[A-Za-z][A-Za-z0-9_']*\\>"
     . font-lock-variable-name-face)
;modules and constructors
   '("\\(\\<\\|:\\)\\([A-Z][A-Za-z0-9_']*\\)\\>"
     2 font-lock-function-name-face)
   '("`[A-Za-z][A-Za-z0-9_']*\\>" . font-lock-function-name-face)
;definition
   (cons (concat
	  "\\<\\(and\\|as\\|c\\(onstraint\\|losed\\)"
	  "\\|ex\\(ception\\|ternal\\)\\|fun\\(ct\\(ion\\|or\\)\\)?"
	  "\\|in\\(herit\\)?\\|let\\|m\\(ethod\\|utable\\|odule\\)"
	  "\\|of\\|p\\(arser\\|rivate\\)\\|rec\\|type"
	  "\\|v\\(al\\(ue\\)?\\|irtual\\)\\)\\>")
	 'font-lock-type-face)
;blocking
   '("\\(\\<\\|:\\)\\(begin\\|class\\|end\\|s\\(ig\\|truct\\)\\)\\>"
     2 font-lock-keyword-face)
;control
   (cons (concat
	  "\\<\\(do\\(ne\\|wnto\\)?\\|else\\|for\\|if"
	  "\\|match\\|new\\|or\\|t\\(hen\\|o\\|ry\\)"
	  "\\|w\\(h\\(en\\|ile\\)\\|ith\\)\\)\\>"
	  "\\|\|\\|->\\|&\\|#")
	 'font-lock-reference-face) 
   '("\\<raise\\>" . font-lock-comment-face)))

(defconst inferior-caml-font-lock-keywords
  (append
   (list
;inferior
    '("^[#-]" . font-lock-comment-face)
;labels
    '("[? \t]:[A-Za-z][A-Za-z0-9_']*\\>" . font-lock-variable-name-face))
   caml-font-lock-keywords))
    
;; font-lock commands are similar for caml-mode and inferior-caml-mode
(setq caml-mode-hook
      '(lambda ()
	 (cond
	  ((fboundp 'global-font-lock-mode)
	   (make-local-variable 'font-lock-defaults)
	   (setq font-lock-defaults
		 '(caml-font-lock-keywords nil nil ((?' . "w") (?_ . "w")))))
	  (t
	   (setq font-lock-keywords caml-font-lock-keywords)))
	 (setq font-lock-no-comments t)
	 (font-lock-mode 1)))

(setq inferior-caml-mode-hooks
      '(lambda ()
	 (cond
	  ((fboundp 'global-font-lock-mode)
	   (make-local-variable 'font-lock-defaults)
	   (setq font-lock-defaults
		 '(inferior-caml-font-lock-keywords
		   nil nil ((?' . "w") (?_ . "w")))))
	  (t
	   (setq font-lock-keywords inferior-caml-font-lock-keywords)))
	 (setq font-lock-no-comments t)
	 (font-lock-mode 1)))

(provide 'caml-font)
