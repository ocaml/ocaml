;;; caml.el --- O'Caml code editing commands for Emacs

;; Xavier Leroy, july 1993.

;;indentation code is Copyright (C) 1996 by Ian T Zimmerman <itz@rahul.net>
;;copying: covered by the current FSF General Public License.

;; indentation code adapted for Objective Caml by Jacques Garrigue,
;; july 1997. <garrigue@kurims.kyoto-u.ac.jp>

;;user customizable variables
(defvar caml-quote-char "'"
  "*Quote for character constants. \"'\" for Objective Caml, \"`\" for Caml-Light.")

(defvar caml-imenu-disable nil
  "*Disable Imenu support.")

(defvar caml-olabl-disable nil
  "*Disable O'Labl support")

(defvar caml-mode-indentation 2
  "*Used for \\[caml-unindent-command].")

(defvar caml-lookback-limit 2000
  "*How far to look back for syntax things in caml mode.")

(defvar caml-max-indent-priority 8
  "*Bounds priority of operators permitted to affect caml indentation.

Priorities are assigned to `interesting' caml operators as follows:

	all keywords 0 to 7	8
	type, val, ... + 0	7
	:: ^			6
	@			5
	:= <-			4
	if			3
	fun, let, match ...	2
	module			1
	opening keywords 	0.")

(defvar caml-apply-extra-indent 2
  "*How many spaces to add to indentation for an application in caml mode.")
(make-variable-buffer-local 'caml-apply-extra-indent)

(defvar caml-begin-indent 2
  "*How many spaces to indent from a begin keyword in caml mode.")
(make-variable-buffer-local 'caml-begin-indent)

(defvar caml-class-indent 2
  "*How many spaces to indent from a class keyword in caml mode.")
(make-variable-buffer-local 'caml-class-indent)

(defvar caml-exception-indent 2
  "*How many spaces to indent from a exception keyword in caml mode.")
(make-variable-buffer-local 'caml-exception-indent)

(defvar caml-for-indent	2
  "*How many spaces to indent from a for keyword in caml mode.")
(make-variable-buffer-local 'caml-for-indent)

(defvar caml-fun-indent	2
  "*How many spaces to indent from a fun keyword in caml mode.")
(make-variable-buffer-local 'caml-fun-indent)

(defvar caml-function-indent 4
  "*How many spaces to indent from a function keyword in caml mode.")
(make-variable-buffer-local 'caml-function-indent)

(defvar caml-if-indent	2
  "*How many spaces to indent from a if keyword in caml mode.")
(make-variable-buffer-local 'caml-if-indent)

(defvar caml-if-else-indent 0
  "*How many spaces to indent from an if .. else line in caml mode.")
(make-variable-buffer-local 'caml-if-else-indent)

(defvar caml-inherit-indent 2
  "*How many spaces to indent from a inherit keyword in caml mode.")
(make-variable-buffer-local 'caml-inherit-indent)

(defvar caml-include-indent 2
  "*How many spaces to indent from a include keyword in caml mode.")
(make-variable-buffer-local 'caml-include-indent)

(defvar caml-let-indent	2
  "*How many spaces to indent from a let keyword in caml mode.")
(make-variable-buffer-local 'caml-let-indent)

(defvar caml-let-in-indent 0
  "*How many spaces to indent from a let .. in keyword in caml mode.")
(make-variable-buffer-local 'caml-let-in-indent)

(defvar caml-match-indent 2
  "*How many spaces to indent from a match keyword in caml mode.")
(make-variable-buffer-local 'caml-match-indent)

(defvar caml-method-indent 2
  "*How many spaces to indent from a method keyword in caml mode.")
(make-variable-buffer-local 'caml-method-indent)

(defvar caml-module-indent 2
  "*How many spaces to indent from a module keyword in caml mode.")
(make-variable-buffer-local 'caml-module-indent)

(defvar caml-of-indent 2
  "*How many spaces to indent from a of keyword in caml mode.")
(make-variable-buffer-local 'caml-of-indent)

(defvar caml-parser-indent 4
  "*How many spaces to indent from a parser keyword in caml mode.")
(make-variable-buffer-local 'caml-parser-indent)

(defvar caml-sig-indent 2
  "*How many spaces to indent from a sig keyword in caml mode.")
(make-variable-buffer-local 'caml-sig-indent)

(defvar caml-struct-indent 2
  "*How many spaces to indent from a struct keyword in caml mode.")
(make-variable-buffer-local 'caml-struct-indent)

(defvar caml-try-indent	2
  "*How many spaces to indent from a try keyword in caml mode.")
(make-variable-buffer-local 'caml-try-indent)

(defvar caml-type-indent 4
  "*How many spaces to indent from a type keyword in caml mode.")
(make-variable-buffer-local 'caml-type-indent)

(defvar caml-val-indent	2
  "*How many spaces to indent from a val keyword in caml mode.")
(make-variable-buffer-local 'caml-val-indent)

(defvar caml-while-indent 2
  "*How many spaces to indent from a while keyword in caml mode.")
(make-variable-buffer-local 'caml-while-indent)

(defvar caml-::-indent	3
  "*How many spaces to indent from a :: operator in caml mode.")
(make-variable-buffer-local 'caml-::-indent)

(defvar caml-@-indent	2
  "*How many spaces to indent from a @ operator in caml mode.")
(make-variable-buffer-local 'caml-@-indent)

(defvar caml-:=-indent  3
  "*How many spaces to indent from a := operator in caml mode.")
(make-variable-buffer-local 'caml-:=-indent)

(defvar caml-<--indent	3
  "*How many spaces to indent from a <- operator in caml mode.")
(make-variable-buffer-local 'caml-<--indent)

(defvar caml-->-indent	2
  "*How many spaces to indent from a -> operator in caml mode.")
(make-variable-buffer-local 'caml-->-indent)

(defvar caml-lb-indent 2
  "*How many spaces to indent from a \[ operator in caml mode.")
(make-variable-buffer-local 'caml-lb-indent)

(defvar caml-lc-indent 2
  "*How many spaces to indent from a \{ operator in caml mode.")
(make-variable-buffer-local 'caml-lc-indent)

(defvar caml-lp-indent	1
  "*How many spaces to indent from a \( operator in caml mode.")
(make-variable-buffer-local 'caml-lp-indent)

(defvar caml-and-extra-indent nil
  "*Extra indent for caml lines starting with the and keyword.
Usually negative. nil is align on master.")
(make-variable-buffer-local 'caml-and-extra-indent)

(defvar caml-do-extra-indent nil
  "*Extra indent for caml lines starting with the do keyword.
Usually negative. nil is align on master.")
(make-variable-buffer-local 'caml-do-extra-indent)

(defvar caml-done-extra-indent nil
  "*Extra indent for caml lines starting with the done keyword.
Usually negative. nil is align on master.")
(make-variable-buffer-local 'caml-done-extra-indent)

(defvar caml-else-extra-indent nil
  "*Extra indent for caml lines starting with the else keyword.
Usually negative. nil is align on master.")
(make-variable-buffer-local 'caml-else-extra-indent)

(defvar caml-end-extra-indent nil
  "*Extra indent for caml lines starting with the end keyword.
Usually negative. nil is align on master.")
(make-variable-buffer-local 'caml-end-extra-indent)

(defvar caml-in-extra-indent nil
  "*Extra indent for caml lines starting with the in keyword.
Usually negative. nil is align on master.")
(make-variable-buffer-local 'caml-in-extra-indent)

(defvar caml-then-extra-indent nil
  "*Extra indent for caml lines starting with the then keyword.
Usually negative. nil is align on master.")
(make-variable-buffer-local 'caml-then-extra-indent)

(defvar caml-to-extra-indent -1
  "*Extra indent for caml lines starting with the to keyword.
Usually negative. nil is align on master.")
(make-variable-buffer-local 'caml-to-extra-indent)

(defvar caml-with-extra-indent nil
  "*Extra indent for caml lines starting with the with keyword.
Usually negative. nil is align on master.")
(make-variable-buffer-local 'caml-with-extra-indent)

(defvar caml-|-extra-indent -2
  "*Extra indent for caml lines starting with the | operator.
Usually negative. nil is align on master.")
(make-variable-buffer-local 'caml-|-extra-indent)

(defvar caml-rb-extra-indent -2
  "*Extra indent for caml lines statring with ].
Usually negative. nil is align on master.")

(defvar caml-rc-extra-indent -2
  "*Extra indent for caml lines starting with }.
Usually negative. nil is align on master.")

(defvar caml-electric-indent t
  "*Non-nil means electrically indent lines starting with |, ] or }.

Many people find eletric keys irritating, so you can disable them if
you are one.")

(defvar caml-electric-close-vector t
  "*Non-nil means electrically insert a | before a vector-closing ].

Many people find eletric keys irritating, so you can disable them if
you are one. You should probably have this on, though, if you also
have caml-electric-indent on, which see.")

;;code
(if (or (not (fboundp 'indent-line-to))
	(not (fboundp 'buffer-substring-no-properties)))
    (require 'caml-compat))

(defvar caml-shell-active nil
  "*Non nil when a subshell is running.")

(defvar caml-mode-map nil
  "Keymap used in Caml mode.")
(if caml-mode-map
    ()
  (setq caml-mode-map (make-sparse-keymap))
  (define-key caml-mode-map "|" 'caml-electric-pipe)
  (define-key caml-mode-map "}" 'caml-electric-pipe)
  (define-key caml-mode-map "]" 'caml-electric-rb)
  (define-key caml-mode-map "\t" 'caml-indent-command)
  (define-key caml-mode-map [backtab] 'caml-unindent-command)

;itz 04-21-96 instead of defining a new function, use defadvice
;that way we get out effect even when we do \C-x` in compilation buffer  
;  (define-key caml-mode-map "\C-x`" 'caml-next-error)

  (define-key caml-mode-map "\177" 'backward-delete-char-untabify)
  (define-key caml-mode-map "\C-cb" 'caml-insert-begin-form)
  (define-key caml-mode-map "\C-cf" 'caml-insert-for-form)
  (define-key caml-mode-map "\C-ci" 'caml-insert-if-form)
  (define-key caml-mode-map "\C-cl" 'caml-insert-let-form)
  (define-key caml-mode-map "\C-cm" 'caml-insert-match-form)
  (define-key caml-mode-map "\C-ct" 'caml-insert-try-form)
  (define-key caml-mode-map "\C-cw" 'caml-insert-while-form)
  (define-key caml-mode-map "\C-c\C-a" 'caml-find-alternate-file)
  (define-key caml-mode-map "\C-c\C-c" 'compile)
  (define-key caml-mode-map "\C-c\C-e" 'caml-eval-phrase)
  (define-key caml-mode-map "\C-c\C-\[" 'caml-backward-to-less-indent)
  (define-key caml-mode-map "\C-c\C-\]" 'caml-forward-to-less-indent)
  (define-key caml-mode-map "\C-c\C-q" 'caml-indent-phrase)
  (define-key caml-mode-map "\C-c\C-r" 'caml-eval-region)
  (define-key caml-mode-map "\C-c\C-s" 'caml-show-subshell)
  (define-key caml-mode-map "\M-\C-h" 'caml-mark-phrase)
  (define-key caml-mode-map "\M-\C-q" 'caml-indent-phrase)
  (define-key caml-mode-map "\M-\C-x" 'caml-eval-phrase)
  (let ((map (make-sparse-keymap "Caml"))
	(forms (make-sparse-keymap "Forms")))
    (define-key caml-mode-map [menu-bar] (make-sparse-keymap))
    (define-key caml-mode-map [menu-bar caml] (cons "Caml" map))
    (define-key map [run-caml] '("Start subshell..." . run-caml))
    (define-key map [compile] '("Compile..." . compile))
    (define-key map [switch-view] '("Switch view" . caml-find-alternate-file))
    (define-key map [separator-format] '("--"))
    (define-key map [forms] (cons "Forms" forms))
    (define-key map [show-subshell] '("Show subshell" . caml-show-subshell))
    (put 'caml-show-subshell 'menu-enable 'caml-shell-active)
    (define-key map [eval-phrase] '("Eval phrase" . caml-eval-phrase))
    (put 'caml-eval-phrase 'menu-enable 'caml-shell-active)
    (define-key map [indent-phrase] '("Indent phrase" . caml-indent-phrase))
    (define-key forms [while]
      '("while .. do .. done" . caml-insert-while-form))
    (define-key forms [try] '("try .. with .." . caml-insert-try-form))
    (define-key forms [match] '("match .. with .." . caml-insert-match-form))
    (define-key forms [let] '("let .. in .." . caml-insert-let-form))
    (define-key forms [if] '("if .. then .. else .." . caml-insert-if-form))
    (define-key forms [begin] '("for .. do .. done" . caml-insert-for-form))
    (define-key forms [begin] '("begin .. end" . caml-insert-begin-form))))


(defvar caml-mode-syntax-table nil
  "Syntax table in use in Caml mode buffers.")
(if caml-mode-syntax-table
    ()
  (setq caml-mode-syntax-table (make-syntax-table))
  ; backslash is an escape sequence
  (modify-syntax-entry ?\\ "\\" caml-mode-syntax-table)
  ; ( is first character of comment start
  (modify-syntax-entry ?\( "()1" caml-mode-syntax-table)
  ; * is second character of comment start,
  ; and first character of comment end
  (modify-syntax-entry ?*  ". 23" caml-mode-syntax-table)
  ; ) is last character of comment end
  (modify-syntax-entry ?\) ")(4" caml-mode-syntax-table)
  ; backquote was a string-like delimiter (for character literals)
  ; (modify-syntax-entry ?` "\"" caml-mode-syntax-table)
  ; quote and underscore are part of words
  (modify-syntax-entry ?' "w" caml-mode-syntax-table)
  (modify-syntax-entry ?_ "w" caml-mode-syntax-table)
  ; : is part of words (labels) in O'Labl
  (if caml-olabl-disable nil 
    (modify-syntax-entry ?: "w" caml-mode-syntax-table))
  ; ISO-latin accented letters and EUC kanjis are part of words
  (let ((i 160))
    (while (< i 256)
      (modify-syntax-entry i "w" caml-mode-syntax-table)
      (setq i (1+ i)))))

(defvar caml-mode-abbrev-table nil
  "Abbrev table used for Caml mode buffers.")
(if caml-mode-abbrev-table nil
  (setq caml-mode-abbrev-table (make-abbrev-table))
  (define-abbrev caml-mode-abbrev-table "and" "and" 'caml-abbrev-hook)
  (define-abbrev caml-mode-abbrev-table "do" "do" 'caml-abbrev-hook)
  (define-abbrev caml-mode-abbrev-table "done" "done" 'caml-abbrev-hook)
  (define-abbrev caml-mode-abbrev-table "else" "else" 'caml-abbrev-hook)
  (define-abbrev caml-mode-abbrev-table "end" "end" 'caml-abbrev-hook)
  (define-abbrev caml-mode-abbrev-table "in" "in" 'caml-abbrev-hook)
  (define-abbrev caml-mode-abbrev-table "then" "then" 'caml-abbrev-hook)
  (define-abbrev caml-mode-abbrev-table "with" "with" 'caml-abbrev-hook))

;;; The major mode

(defun caml-mode ()
  "Major mode for editing Caml code.

\\{caml-mode-map}"

  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'caml-mode)
  (setq mode-name "caml")
  (use-local-map caml-mode-map)
  (set-syntax-table caml-mode-syntax-table)
  (setq local-abbrev-table caml-mode-abbrev-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'comment-start)
  (setq comment-start "(*")
  (make-local-variable 'comment-end)
  (setq comment-end "*)")
  (make-local-variable 'comment-column)
  (setq comment-column 40)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "(\\*+ *")
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments nil)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'caml-indent-command)
  ;itz 03-25-96
  (setq before-change-function 'caml-before-change-function)
  (setq caml-last-noncomment-pos nil)
  (setq caml-last-comment-start (make-marker))
  (setq caml-last-comment-end (make-marker))
  ;garrigue 27-11-96
  (setq case-fold-search nil)
  ;imenu support
  (make-local-variable 'imenu-create-index-function)
  (setq imenu-create-index-function 'caml-create-index-function)
  (if caml-imenu-disable nil
    (require 'imenu)
    (imenu-add-to-menubar "Defs"))
  (run-hooks 'caml-mode-hook))

;;; Auxiliary function. Garrigue 96-11-01.

(defun caml-find-alternate-file ()
  (interactive)
  (let ((name (buffer-file-name)))
    (if (string-match "^\\(.*\\)\\.\\(ml\\|mli\\)$" name)
	(find-file
	 (concat
	  (caml-match-string 1 name)
	  (if (string= "ml" (caml-match-string 2 name)) ".mli" ".ml"))))))

;;; subshell support

(defun caml-eval-phrase ()
  "Send the current Caml phrase to the inferior Caml process."
  (interactive)
  (save-excursion
    (let ((bounds (caml-mark-phrase)))
    (inferior-caml-eval-region (car bounds) (cdr bounds)))))

(defun caml-show-subshell () (inferior-caml-show-subshell))

;;; Imenu support
(defconst caml-imenu-search-regexp
  (concat "\\<in\\>\\|"
	  "^[ \t]*\\(let\\|class\\|type\\|m\\(odule\\|ethod\\)"
	  "\\|functor\\|and\\|val\\)[ \t]+"
	  "\\(\\('[a-zA-Z0-9]+\\|([^)]+)"
	  "\\|mutable\\|private\\|rec\\|type\\)[ \t]+\\)?"
	  "\\([a-zA-Z][a-zA-Z0-9_']*\\)"))
(defun caml-prev-index-position-function ()
  (let (found data)
    (while (and (setq found
		      (re-search-backward caml-imenu-search-regexp nil 'move))
		(progn (setq data (match-data)) t)
		(or (caml-in-literal-p)
		    (caml-in-comment-p)
		    (if (looking-at "in") (caml-find-in-match)))))
    (set-match-data data)
    found))
(defun caml-create-index-function ()
  (let (value-alist
	type-alist
	class-alist
	method-alist
	module-alist
	and-alist
	all-alist
	menu-alist
	prev-pos
	index)
    (goto-char (point-max))
    (imenu-progress-message prev-pos 0 t)
    ;; collect definitions
    (while (caml-prev-index-position-function)
      (setq index (cons (caml-match-string 5) (point)))
      (imenu-progress-message prev-pos nil t)
      (setq all-alist (cons index all-alist))
      (cond
       ((looking-at "[ \t]*and")
	(setq and-alist (cons index and-alist)))
       ((looking-at "[ \t]*let")
	(setq value-alist (cons index (append and-alist value-alist)))
	(setq and-alist nil))
       ((looking-at "[ \t]*type")
	(setq type-alist (cons index (append and-alist type-alist)))
	(setq and-alist nil))
       ((looking-at "[ \t]*class")
	(setq class-alist (cons index (append and-alist class-alist)))
	(setq and-alist nil))
       ((looking-at "[ \t]*val")
	(setq value-alist (cons index value-alist)))
       ((looking-at "[ \t]*\\(module\\|functor\\)")
	(setq module-alist (cons index module-alist)))
       ((looking-at "[ \t]*method")
	(setq method-alist (cons index method-alist)))))
    ;; build menu
    (mapcar
     '(lambda (pair)
	(if (symbol-value (cdr pair))
	    (setq menu-alist
		  (cons
		   (cons (car pair)
			 (sort (symbol-value (cdr pair)) 'imenu--sort-by-name))
		   menu-alist))))
     '(("Values" . value-alist)
       ("Types" . type-alist)
       ("Modules" . module-alist)
       ("Methods" . method-alist)
       ("Classes" . class-alist)
       ("Index" . all-alist)))
    (imenu-progress-message prev-pos 100 t)
    menu-alist))

;;; Indentation stuff

(defun caml-in-indentation ()
  "Tests whether all characters between beginning of line and point
are blanks."
  (save-excursion
    (skip-chars-backward " \t")
    (bolp)))

;;; The command
;;; Sorry, I didn't like the previous behaviour... Garrigue 96/11/01

(defun caml-indent-command (&optional p)
  "Indent the current line in Caml mode.

Compute new indentation based on caml syntax. If prefixed, indent
the line all the way to where point is."

  (interactive "*p")
  (cond
   ((and p (> p 1)) (indent-line-to (current-column)))
   ((caml-in-indentation) (indent-line-to (caml-compute-final-indent)))
   (t (save-excursion
	(indent-line-to
	 (caml-compute-final-indent))))))
   
(defun caml-unindent-command ()

  "Decrease indentation by one level in Caml mode.  

Works only if the point is at the beginning of an indented line
\(i.e. all characters between beginning of line and point are
blanks\).  Does nothing otherwise. The unindent size is given by the
variable caml-mode-indentation."

  (interactive "*")
  (let* ((begline
          (save-excursion
            (beginning-of-line)
            (point)))
         (current-offset
          (- (point) begline)))
    (if (and (>= current-offset caml-mode-indentation)
             (caml-in-indentation))
        (backward-delete-char-untabify caml-mode-indentation))))

;;; Error processing

(require 'compile)

;; In Emacs 19, the regexps in compilation-error-regexp-alist do not
;; match the error messages when the language is not English.
;; Hence we add a regexp.

(defconst caml-error-regexp
  "^[A-\377]+ \"\\([^\"\n]+\\)\", [A-\377]+ \\([0-9]+\\)[-,:]"
  "Regular expression matching the error messages produced by camlc.")

(if (boundp 'compilation-error-regexp-alist)
    (or (assoc caml-error-regexp
               compilation-error-regexp-alist)
        (setq compilation-error-regexp-alist
              (cons (list caml-error-regexp 1 2)
               compilation-error-regexp-alist))))

;; A regexp to extract the range info

(defconst caml-error-chars-regexp
  ".*, .*, [A-\377]+ \\([0-9]+\\)-\\([0-9]+\\):"
  "Regular expression extracting the character numbers
from an error message produced by camlc.")

;; Wrapper around next-error.

(defvar caml-error-overlay nil)

;;itz 04-21-96 somebody didn't get the documetation for next-error
;;right. When the optional argument is a number n, it should move
;;forward n errors, not reparse.

;itz 04-21-96 instead of defining a new function, use defadvice
;that way we get our effect even when we do \C-x` in compilation buffer  

(defadvice next-error (after caml-next-error activate)
 "Reads the extra positional information provided by the Caml compiler.

Puts the point and the mark exactly around the erroneous program
fragment. The erroneous fragment is also temporarily highlighted if
possible."

 (if (eq major-mode 'caml-mode)
     (let ((beg nil) (end nil))
       (save-excursion
	 (set-buffer
	  (if (boundp 'compilation-last-buffer) 
	      compilation-last-buffer	;Emacs 19
	    "*compilation*"))		;Emacs 18
	 (save-excursion
	   (goto-char (window-point (get-buffer-window (current-buffer))))
	   (if (looking-at caml-error-chars-regexp)
	       (setq beg
		     (string-to-int
		      (buffer-substring (match-beginning 1) (match-end 1)))
		     end
		     (string-to-int
		      (buffer-substring (match-beginning 2) (match-end 2)))))))
       (cond (beg
	      (setq beg (+ (point) beg)
		    end (+ (point) end))
	      (goto-char beg)
	      (push-mark end t)
	      (cond ((fboundp 'make-overlay)
		     (if caml-error-overlay ()
		       (setq caml-error-overlay (make-overlay 1 1))
		       (overlay-put caml-error-overlay 'face 'region))
		     (unwind-protect
			 (progn
			   (move-overlay caml-error-overlay
					 beg end (current-buffer))
			   (sit-for 60))
		       (delete-overlay caml-error-overlay)))))))))

;; Usual match-string doesn't work properly with font-lock-mode
;; on some emacs.

(defun caml-match-string (num &optional string)

  "Return string of text matched by last search, without properties.

NUM specifies which parenthesized expression in the last regexp.
Value is nil if NUMth pair didn't match, or there were less than NUM
pairs.  Zero means the entire text matched by the whole regexp or
whole string."

  (let* ((data (match-data))
	 (begin (nth (* 2 num) data))
	 (end (nth (1+ (* 2 num)) data)))
    (if string (substring string begin end)
      (buffer-substring-no-properties begin end))))

;;; Phrases

;itz the heuristics used to see if we're `between two phrases'
;didn't seem right to me.

(defconst caml-phrase-start-keywords
  (concat "\\<\\(class\\|ex\\(ternal\\|ception\\)\\|functor"
	  "\\|let\\|module\\|open\\|type\\|val\\)\\>")
  "Keywords starting phrases in files")

;; a phrase starts when a toplevel keyword is at the beginning of a line
(defun caml-at-phrase-start-p ()
  (and (bolp)
       (or (looking-at "#")
	   (looking-at caml-phrase-start-keywords))))

(defun caml-mark-phrase ()
  "Put mark at end of this Caml phrase, point at beginning.

The Caml phrase is the phrase just before the point.
Completely rewritten by J. Garrigue, to handle both Objective Caml
and Caml-Light syntax. \";;\" is left out of the region, and
eventually added when sending to the subprocess."

  (interactive)
  (let (use-semi end)
    (if (and (looking-at ";;") (not (caml-in-comment-p))) nil
      (if (caml-at-phrase-start-p) (forward-char))
      (while (and (cond
		   ((re-search-forward
		     (concat ";;\\|" caml-phrase-start-keywords) nil 'move)
		    (goto-char (match-beginning 0)) t))
		  (or (not (or (bolp) (looking-at ";;")))
		      (caml-in-comment-p)
		      (caml-in-literal-p)))
	(forward-char)))
    (setq use-semi (looking-at ";;"))
    (skip-chars-backward " \n\t")
    (while (and (eq (preceding-char) ?\)) (eq (char-after (- (point) 2)) ?*))
      (backward-char 2)
      (while (caml-in-comment-p) (up-list -1))
      (skip-chars-backward " \n\t"))
    (push-mark)
    (setq end (point))
    (cond
     (use-semi
      (if (caml-find-kwop ";;") (forward-char 2)
	(goto-char (point-min)))
      (skip-chars-forward " \n\t")
      (while (or (looking-at comment-start-skip) (caml-in-comment-p))
	(forward-list 1)
	(skip-chars-forward " \n\t")))
     (t
      (if (not (caml-find-kwop caml-phrase-start-keywords))
	  (error "No phrase preceding point"))
      (while (and (or (not (bolp))
		      (caml-in-comment-p)
		      (caml-in-literal-p))
		  (caml-find-kwop caml-phrase-start-keywords)))))
    (cons (point) end)))

(defvar caml-last-noncomment-pos nil
  "Caches last buffer position determined not inside a caml comment.")
(make-variable-buffer-local 'caml-last-noncomment-pos)

;;last-noncomment-pos can be a simple position, because we nil it
;;anyway whenever buffer changes upstream. last-comment-start and -end
;;have to be markers, because we preserve them when the changes' end
;;doesn't overlap with the comment's start.

(defvar caml-last-comment-start nil
  "A marker caching last determined caml comment start.")
(make-variable-buffer-local 'caml-last-comment-start)

(defvar caml-last-comment-end nil
  "A marker caching last determined caml comment end.")
(make-variable-buffer-local 'caml-last-comment-end)

(make-variable-buffer-local 'before-change-function)

(defun caml-overlap (b1 e1 b2 e2)
  (<= (max b1 b2) (min e1 e2)))

;this clears the last comment cache if necessary
(defun caml-before-change-function (begin end)
  (if (and caml-last-noncomment-pos
	   (> caml-last-noncomment-pos begin))
      (setq caml-last-noncomment-pos nil))
  (if (and (marker-position caml-last-comment-start)
	   (marker-position caml-last-comment-end)
	   (caml-overlap begin end
			 caml-last-comment-start
			 caml-last-comment-end))
      (prog2
	  (set-marker caml-last-comment-start nil)
	  (set-marker caml-last-comment-end nil)))
  (let ((orig-function (default-value 'before-change-function)))
    (if orig-function (funcall orig-function begin end))))

(defun caml-in-literal-p ()
  "Returns non-nil if point is inside a caml literal."
  (let* ((start-literal (concat "[\"" caml-quote-char "]"))
	 (char-literal
	  (concat "\\([^\\]\\|\\\\\\.\\|\\\\[0-9][0-9][0-9]\\)"
		  caml-quote-char))
	 (pos (point))
	 (eol (progn (end-of-line 1) (point)))
	 state in-str)
    (beginning-of-line 1)
    (while (and (not state)
		(re-search-forward start-literal eol t)
		(<= (point) pos))
      (cond
       ((string= (caml-match-string 0) "\"")
	(setq in-str t)
	(while (and in-str (not state)
		    (re-search-forward "\"\\|\\\\\"" eol t))
	  (if (> (point) pos) (setq state t))
	  (if (string= (caml-match-string 0) "\"") (setq in-str nil)))
	(if in-str (setq state t)))
       ((looking-at char-literal)
	(if (and (>= pos (match-beginning 0)) (< pos (match-end 0)))
	    (setq state t)
	  (goto-char (match-end 0))))))
    (goto-char pos)
    state))

(defun caml-forward-list-safe ()
  (condition-case nil
      (forward-list 1)
    (error (goto-char (point-max)))))



(defun caml-in-comment-p ()
  "Returns non-nil if point is inside a caml comment."
  ;;we look for comments differently than literals. there are two
  ;;reasons for this. first, caml has nested comments and it is not so
  ;;clear that parse-partial-sexp supports them; second, if proper
  ;;style is used, literals are never split across lines, so we don't
  ;;have to worry about bogus phrase breaks inside literals, while we
  ;;have to account for that possibility in comments.
  (save-excursion
    (let* ((cached-pos caml-last-noncomment-pos)
	   (cached-begin (marker-position caml-last-comment-start))
	   (cached-end (marker-position caml-last-comment-end)))
      (cond
       ((and cached-begin cached-end
	     (< cached-begin (point)) (< (point) cached-end)) t)
       ((and cached-pos (= cached-pos (point))) nil)
       ((and cached-pos (< cached-pos (point)))
	(let ((inside) (done nil) (here (point)))
	  (goto-char cached-pos)
	  (while (not done)
	    (setq inside nil)
	    (if (not (search-forward "(*" (1+ here) 'move))
		(setq here 0)
	      (goto-char (match-beginning 0))
	      (setq inside (point))
	      (caml-forward-list-safe))
	    (setq done (< here (point))))
	  (if (not inside)
	      (setq caml-last-noncomment-pos here)
	    (set-marker caml-last-comment-start inside)
	    (set-marker caml-last-comment-end (point)))
	  inside))
       (cached-pos
	(let ((inside) (done nil) (here (point)))
	  (goto-char cached-pos)
	  (while (not done)
	    (setq inside nil)
	    (if (not (search-backward "*)" (1- here) 'move))
		(setq here (point-max))
	      (goto-char (match-end 0))
	      (setq inside (point))
	      (backward-list 1))
	    (setq done (> here (point))))
	  (if (not inside)
	      (setq caml-last-noncomment-pos here)
	    (set-marker caml-last-comment-start (point))
	    (set-marker caml-last-comment-end inside))
	  inside))	    
       (t   
	(let* ((here (point)) (done nil)
	       (limit (- (point) caml-lookback-limit))
	       (begin (condition-case nil
			  (prog2
			      (while (and (not done) (< limit (point)))
				(up-list -1)
				(setq done (looking-at comment-start-skip)))
			      (if done (point)))
			(error nil))))
	  (if (not begin) (setq caml-last-noncomment-pos here) 
	    (goto-char begin)
	    (caml-forward-list-safe)
	    (set-marker caml-last-comment-start begin)
	    (set-marker caml-last-comment-end (point)))
	  begin))))))

(defconst caml-before-expr-prefix
  (concat "\\<\\(asr\\|begin\\|class\\|do\\(wnto\\)?\\|else"
	  "\\|i\\(f\\|n\\(herit\\)?\\)"
	  "\\|f\\(or\\|un\\(ct\\(ion\\|or\\)\\)?\\)"
	  "\\|l\\(and\\|or\\|s[lr]\\|xor\\)\\|m\\(atch\\|od\\)"
	  "\\|o[fr]\\|parser\\|s\\(ig\\|truct\\)\\|t\\(hen\\|o\\|ry\\)"
	  "\\|w\\(h\\(en\\|ile\\)\\|ith\\)\\)\\>\\|:begin\\>"
	  "\\|[=<>@^|&+-*/$%][!$%*+-./:<=>?@^|~]*\\|:[:=]\\|[[({,;]")

  "Keywords that may appear immediately before an expression.
Used to distinguish it from toplevel let construct.")

(defun caml-in-expr-p ()
  (let ((pos (point)) (in-expr t))
    (caml-find-kwop
     (concat caml-before-expr-prefix "\\|"
	     caml-matching-kw-regexp "\\|"
	     (aref caml-kwop-regexps caml-max-indent-priority)))
    (cond
     ; special case for ;;
     ((and (= (preceding-char) ?\;) (= (following-char) ?\;)))
     ((looking-at caml-before-expr-prefix)
      (goto-char (match-end 0))
      (skip-chars-forward " \t\n")
      (while (looking-at "(\\*")
	(forward-sexp 1)
	(skip-chars-forward " \t\n"))
      (if (<= pos (point)) (setq in-expr nil))))
    (goto-char pos)
    in-expr))

(defun caml-at-top-let-p ()
  (and (looking-at "\\<let\\>")
       (or (bolp)
	   (caml-in-expr-p))))


(defun caml-at-sexp-close-p ()
  (or (char-equal ?\) (following-char))
      (char-equal ?\] (following-char))
      (char-equal ?} (following-char))))

(defun caml-find-kwop (kwop-regexp)
  "Look back for a caml keyword or operator matching KWOP-REGEXP.

Ignore occurences inside literals. If found, return a list of two
values: the actual text of the keyword or operator, and a boolean
indicating whether the keyword was one we looked for explicitly
{non-nil}, or on the other hand one of the block-terminating
keywords."
  
  (let ((start-literal (concat "[\"" caml-quote-char "]"))
	found kwop)
    (progn
      (while (and (not found)
		  (re-search-backward kwop-regexp nil t))
	(setq kwop (caml-match-string 0))
	(cond
	 ((caml-in-comment-p)
	  (up-list -1))
	 ((looking-at "(\\*")
	  (backward-char))
	 ((looking-at start-literal))
	 ((caml-in-literal-p)
	  (re-search-backward start-literal))  ;ugly hack
	 ((setq found t)))))
    (if found
	(if (not (string-match "\\`[^|[]|[^]|]?\\'" kwop)) ;arrrrgh!!
	    kwop
	  (forward-char 1) "|") nil)))

;  Association list of indentation values based on governing keywords.
;
;Each element is of the form (KEYWORD OP-TYPE PRIO INDENT). OP-TYPE is
;non-nil for operator-type nodes, which affect indentation in a
;different way from keywords: subsequent lines are indented to the
;actual occurrence of an operator, but relative to the indentation of
;the line where the governing keyword occurs.

(defconst caml-no-indent 0)

(defconst caml-kwop-alist
  '(("begin" 		nil	6	caml-begin-indent)
    (":begin" 		nil	6	caml-begin-indent) ; hack
    ("class" 		nil	0	caml-class-indent)
    ("constraint"	nil	0	caml-val-indent)
    ("sig" 		nil	1	caml-sig-indent)
    ("struct" 		nil	1	caml-struct-indent)
    ("exception" 	nil	0	caml-exception-indent)
    ("for"		nil	6	caml-for-indent)
    ("fun"		nil	3	caml-fun-indent)
    ("function"		nil	3	caml-function-indent)
    ("if"		nil	6	caml-if-indent)
    ("if-else"		nil	6	caml-if-else-indent)
    ("include"		nil	0	caml-include-indent)
    ("inherit"		nil	0	caml-inherit-indent)
    ("let"		nil	6	caml-let-indent)
    ("let-in"		nil	6	caml-let-in-indent)
    ("match"		nil	6	caml-match-indent)
    ("method"		nil	0	caml-method-indent)
    ("module"		nil	0	caml-module-indent)
    ("of"		nil	7	caml-of-indent)
    ("open"		nil	0	caml-no-indent)
    ("parser"		nil	3	caml-parser-indent)
    ("try"		nil	6	caml-try-indent)
    ("type"		nil	0	caml-type-indent)
    ("val"		nil	0	caml-val-indent)
    ("virtual"		nil	0	caml-val-indent)
    ("while"		nil	6	caml-while-indent)
    ("::"		t	5	caml-::-indent)
    ("@"		t	4	caml-@-indent)
    ("^"		t	4	caml-@-indent)
    (":="		nil	3	caml-:=-indent)
    ("<-"		nil	3	caml-<--indent)
    ("->"		nil	2	caml-->-indent)
    ("\["		t	8	caml-lb-indent)
    ("{"		t	8	caml-lc-indent)
    ("\("		t	8	caml-lp-indent)
    ("|"		nil	2	caml-no-indent))
; if-else and let-in are not keywords but idioms
; "|" is not in the regexps
; all these 3 values correspond to hard-coded names

"Association list of indentation values based on governing keywords.

Each element is of the form (KEYWORD OP-TYPE PRIO INDENT). OP-TYPE is
non-nil for operator-type nodes, which affect indentation in a
different way from keywords: subsequent lines are indented to the
actual occurrence of an operator, but relative to the indentation of
the line where the governing keyword occurs.")

;;Originally, we had caml-kwop-regexp create these at runtime, from an
;;additional field in caml-kwop-alist. That proved way too slow,
;;although I still can't understand why. itz

(defconst caml-kwop-regexps (make-vector 9 nil)
  "Array of regexps representing caml keywords of different priorities.")

(aset caml-kwop-regexps 0
      (concat 
       "\\<\\(begin\\|class\\|for\\|s\\(ig\\|truct\\)\\|while\\)\\>"
       "\\|:begin\\>\\|[[({]"))
(aset caml-kwop-regexps 1
      (concat (aref caml-kwop-regexps 0) "\\|\\<module\\>"))
(aset caml-kwop-regexps 2
      (concat
       (aref caml-kwop-regexps 1)
       "\\|\\<\\(fun\\(ction\\)?\\|let\\|m\\(atch\\|ethod\\)"
       "\\|parser\\|try\\|val\\)\\>\\|->"))
(aset caml-kwop-regexps 3
      (concat (aref caml-kwop-regexps 2) "\\|\\<if\\>"))
(aset caml-kwop-regexps 4
      (concat (aref caml-kwop-regexps 3) "\\|:=\\|<-"))
(aset caml-kwop-regexps 5
      (concat (aref caml-kwop-regexps 4) "\\|@"))
(aset caml-kwop-regexps 6
      (concat (aref caml-kwop-regexps 5) "\\|::\\|\\^"))
(aset caml-kwop-regexps 7
      (concat
       (aref caml-kwop-regexps 0)
       "\\|\\<\\(constraint\\|exception\\|in\\(herit\\|clude\\)"
       "\\|o\\(f\\|pen\\)\\|type\\|v\\(al\\|irtual\\)\\)\\>"))
(aset caml-kwop-regexps 8
      (concat (aref caml-kwop-regexps 6)
       "\\|\\<\\(constraint\\|exception\\|in\\(herit\\|clude\\)"
       "\\|o\\(f\\|pen\\)\\|type\\|virtual\\)\\>"))

(defun caml-find-done-match ()
  (let ((unbalanced 1) (kwop t))
    (while (and (not (= 0 unbalanced)) kwop)
      (setq kwop (caml-find-kwop "\\<\\(done\\|for\\|while\\)\\>"))
      (cond
       ((not kwop))
       ((string= kwop "done") (setq unbalanced (1+ unbalanced)))
       (t (setq unbalanced (1- unbalanced)))))
    kwop))
      
(defun caml-find-end-match ()
  (let ((unbalanced 1) (kwop t))
    (while (and (not (= 0 unbalanced)) kwop)
      (setq kwop
	    (caml-find-kwop
	     "\\<\\(end\\|begin\\|class\\|s\\(ig\\|truct\\)\\)\\>\\|:begin\\>\\|;;"))
      (cond
       ((not kwop))
       ((string= kwop ";;") (setq kwop nil) (forward-line 1))
       ((string= kwop "end") (setq unbalanced (1+ unbalanced)))
       ( t (setq unbalanced (1- unbalanced)))))
    (if (string= kwop ":begin") "begin"
      kwop)))

(defun caml-find-in-match ()
  (let ((unbalanced 1) (kwop t))
    (while (and (not (= 0 unbalanced)) kwop)
      (setq kwop (caml-find-kwop "\\<\\(in\\|let\\)\\>"))
      (cond
       ((not kwop))
       ((string= kwop "in") (setq unbalanced (1+ unbalanced)))
       ( t (setq unbalanced (1- unbalanced)))))
    kwop))
  
(defun caml-find-with-match ()
  (let ((unbalanced 1) (kwop t))
    (while (and (not (= 0 unbalanced)) kwop)
      (setq kwop (caml-find-kwop
		  "\\<\\(with\\|try\\|m\\(atch\\|odule\\)\\|functor\\)\\>"))
      (cond
       ((not kwop))
       ((or (string= kwop "module") (string= kwop "functor"))
	(setq unbalanced 0))
       ((string= kwop "with") (setq unbalanced (1+ unbalanced)))
       ( t (setq unbalanced (1- unbalanced)))))
    kwop))

(defun caml-find-then-match (&optional from-else)
  (let ((bol (if from-else
		 (save-excursion
		   (progn (beginning-of-line) (point)))))
	kwop done matching-fun)
    (while (not done)
      (setq kwop (caml-find-kwop
		  "\\<\\(e\\(nd\\|lse\\)\\|done\\|then\\|if\\)\\>\\|[])};]"))
      (cond
       ((not kwop) (setq done t))
       ((caml-at-sexp-close-p)
	(forward-char 1)
	(backward-list 1))
       ((string= kwop "if") (setq done t))
       ((string= kwop "then")
	(if (not from-else) (setq kwop (caml-find-then-match))))
       ((setq matching-fun (cdr-safe (assoc kwop caml-matching-kw-alist)))
	(setq kwop (funcall matching-fun)))
       ((string= kwop "then")
	(if (not from-else) (setq kwop (caml-find-then-match))))))
    (if (and bol (>= (point) bol))
	"if-else"
      kwop)))

(defun caml-find-pipe-match ()
  (let ((done nil) (kwop)
	(re (concat
	     "\\<\\(try\\|match\\|with\\|function\\|parser\\|type"
	     "\\|e\\(nd\\|lse\\)\\|done\\|then\\|in\\)\\>"
	     "\\|[^[|]|\\|[])}]")))
    (while (not done)
      (setq kwop (caml-find-kwop re))
      (cond
       ((not kwop) (setq done t))
       ((looking-at "[^[|]\\(|\\)")
	(goto-char (match-beginning 1))
	(setq kwop "|")
	(setq done t))
       ((caml-at-sexp-close-p)
	(forward-char 1)
	(backward-list 1))
       ((string= kwop "with")
	(setq kwop (caml-find-with-match))
	(setq done t))
       ((string= kwop "done") (caml-find-done-match))
       ((string= kwop "end") (caml-find-end-match))
       ((string= kwop "then") (caml-find-then-match))
       ((string= kwop "else") (caml-find-else-match))
       ((string= kwop "in") (caml-find-in-match))
       (t (setq done t))))
    kwop))

(defun caml-find-and-match ()
  (let ((done nil) (kwop))
    (while (not done)
      (setq kwop (caml-find-kwop
		  "\\<\\(class\\|exception\\|let\\|type\\|end\\|in\\)\\>"))
      (cond
       ((not kwop) (setq done t))
       ((string= kwop "end") (caml-find-end-match))
       ((string= kwop "in") (caml-find-in-match))
       (t (setq done t))))
    kwop))

(defun caml-find-else-match ()
  (caml-find-then-match t))

(defun caml-find-semi-match ()
  (caml-find-kwop-skipping-blocks 2))

(defun caml-find-comma-match ()
  (caml-find-kwop-skipping-blocks 3))

(defconst caml-matching-kw-regexp
  (concat
   "\\<\\(and\\|do\\(ne\\)?\\|e\\(lse\\|nd\\)\\|in\\|t\\(hen\\|o\\)"
   "\\|with\\)\\>\\|[^[|]|")
  "Regexp used in caml mode for skipping back over nested blocks.")

(defconst caml-matching-kw-alist
  '(("|" . caml-find-pipe-match)
    (";" . caml-find-semi-match)
    ("," . caml-find-comma-match)
    ("end" . caml-find-end-match)
    ("done" . caml-find-done-match)
    ("in"  . caml-find-in-match)
    ("with" . caml-find-with-match)
    ("else" . caml-find-else-match)
    ("then" . caml-find-then-match)
    ("to" . caml-find-done-match)
    ("do" . caml-find-done-match)
    ("and" . caml-find-and-match))

  "Association list used in caml mode for skipping back over nested blocks.")

(defun caml-find-kwop-skipping-blocks (prio)
  "Look back for a caml keyword matching caml-kwop-regexps [PRIO].

 Skip nested blocks."

  (let ((done nil) (kwop nil) (matching-fun)
	(kwop-list (aref caml-kwop-regexps prio)))
    (while (not done)
      (setq kwop (caml-find-kwop
		  (concat caml-matching-kw-regexp
			  (cond ((> prio 3) "\\|[])},;]\\|")
				((> prio 2) "\\|[])};]\\|")
				(t "\\|[])}]\\|"))
			  kwop-list)))
      (cond
       ((not kwop) (setq done t))
       ((caml-at-sexp-close-p)
	(forward-char 1)
	(backward-list 1))
       ((and (>= prio 2) (string= kwop "|")) (setq done t))
       ((string= kwop "end") (caml-find-end-match))
       ((string= kwop "done") (caml-find-done-match))
       ((string= kwop "in")
	(cond ((and (caml-find-in-match) (>= prio 2))
	       (setq kwop "let-in")
	       (setq done t))))
       ((setq matching-fun (cdr-safe (assoc kwop caml-matching-kw-alist)))
	(setq kwop (funcall matching-fun))
	(if (looking-at kwop-list) (setq done t)))
       (t (let* ((kwop-info (assoc kwop caml-kwop-alist))
		 (is-op (nth 1 kwop-info)))
	    (if (and is-op (looking-at 
			    (concat (regexp-quote kwop)
				    "|?[ \t]*\\(\n\\|(\\*\\)")))
		(setq kwop-list
		      (aref caml-kwop-regexps (nth 2 kwop-info)))
	      (setq done t))))))
    kwop))

(defun caml-compute-basic-indent (prio)
  "Compute indent of current caml line, ignoring leading keywords.

Find the `governing node' for current line. Compute desired
indentation based on the node and the indentation alists.
Assumes point is exactly at line indentation.
Does not preserve point."
  
  (let* (in-expr
	 (kwop (cond
		((looking-at "|\\([^]|]\\|\\'\\)")
		 (caml-find-pipe-match))
		((and (looking-at caml-phrase-start-keywords)
		      (caml-in-expr-p))
		 (caml-find-end-match))
;		((caml-at-top-let-p)
;		 (caml-find-kwop-skipping-blocks 0))
		((and (looking-at caml-matching-kw-regexp)
		      (assoc (caml-match-string 0) caml-matching-kw-alist))
		 (funcall (cdr-safe (assoc (caml-match-string 0)
				      caml-matching-kw-alist))))
		((looking-at
		  (aref caml-kwop-regexps caml-max-indent-priority))
		 (let* ((kwop (caml-match-string 0))
			(kwop-info (assoc kwop caml-kwop-alist))
			(is-op (if kwop-info (nth 1 kwop-info)))
			(prio (if kwop-info (nth 2 kwop-info)
				caml-max-indent-priority)))
		   (if (and (looking-at (aref caml-kwop-regexps 0))
			    (not (looking-at "class"))
			    (caml-in-expr-p))
		       (setq in-expr t))
		   (caml-find-kwop-skipping-blocks prio)))
		(t
		 (if (and (= prio caml-max-indent-priority) (caml-in-expr-p))
		     (setq in-expr t))
		 (caml-find-kwop-skipping-blocks prio))))
	 (kwop-info (assoc kwop caml-kwop-alist))
	 (indent-diff
	  (cond
	   ((not kwop-info) (beginning-of-line 1) 0)
	   ((nth 1 kwop-info) (symbol-value (nth 3 kwop-info)))
	   (t (back-to-indentation)
	      (let* ((indent (symbol-value (nth 3 kwop-info)))
		     (kwop-extra
		      (if (looking-at "|")
			  (assoc (caml-match-string 0)
				 caml-leading-kwops-alist))))
		(if kwop-extra
		    (- indent (symbol-value (nth 1 kwop-extra)))
		  indent)))))
	 (extra (if in-expr caml-apply-extra-indent 0)))
	 (+ indent-diff extra (current-column))))

(defconst caml-leading-kwops-regexp
  (concat
   "\\<\\(and\\|do\\(ne\\)?\\|e\\(lse\\|nd\\)\\|in"
   "\\|t\\(hen\\|o\\)\\|with\\)\\>\\|[]|}]")

  "Regexp matching caml keywords which need special indentation.")

(defconst caml-leading-kwops-alist
  '(("and" caml-and-extra-indent 2)
    ("do" caml-do-extra-indent 0)
    ("done" caml-done-extra-indent 0)
    ("else" caml-else-extra-indent 3)
    ("end" caml-end-extra-indent 0)
    ("in" caml-in-extra-indent 2)
    ("then" caml-then-extra-indent 3)
    ("to" caml-to-extra-indent 0)
    ("with" caml-with-extra-indent 2)
    ("|" caml-|-extra-indent 2)
    ("]" caml-rb-extra-indent 0)
    ("}" caml-rc-extra-indent 0))

  "Association list of special caml keyword indent values.

Each member is of the form (KEYWORD EXTRA-INDENT PRIO) where
EXTRA-INDENT is the variable holding extra indentation amount for
KEYWORD (usually negative) and PRIO is upper bound on priority of
matching nodes to determine KEYWORD's final indentation.")

(defun caml-compute-final-indent ()
  (save-excursion
    (back-to-indentation)
    (cond
     ((looking-at comment-start-skip)
      (current-column))
     ((caml-in-comment-p)
      (let ((done nil))
	(while (not done)
	  (up-list -1)
	  (setq done (not (caml-in-comment-p))))
	(looking-at comment-start-skip)
	(goto-char (match-end 0))
	(current-column)))
;     ((caml-at-phrase-start-p) 0)
     (t (let* ((leading (looking-at caml-leading-kwops-regexp))
	       (assoc-val (if leading (assoc (caml-match-string 0)
					     caml-leading-kwops-alist)))
	       (extra (if leading (symbol-value (nth 1 assoc-val)) 0))
	       (prio (if leading (nth 2 assoc-val)
		       caml-max-indent-priority))
	       (basic (caml-compute-basic-indent prio)))
	  (max 0 (if extra (+ extra basic) (current-column))))))))
		    


(defun caml-split-string ()
  "Called whenever a line is broken inside a caml string literal."
  (insert-before-markers "\"^\"")
  (backward-char 1))

(defadvice indent-new-comment-line (around
				    caml-indent-new-comment-line
				    activate)
  
  "Handle multi-line strings in caml mode."

;this advice doesn't make sense in other modes. I wish there were a
;cleaner way to do this: I haven't found one.

  (let ((hooked (and (eq major-mode 'caml-mode) (caml-in-literal-p)))
	(split-mark))
    (if (not hooked) nil
      (setq split-mark (set-marker (make-marker) (point)))
      (caml-split-string))
    ad-do-it
    (if (not hooked) nil
      (goto-char split-mark)
      (set-marker split-mark nil))))  
  
(defadvice newline-and-indent (around
			       caml-newline-and-indent
			       activate)

  "Handle multi-line strings in caml mode."

    (let ((hooked (and (eq major-mode 'caml-mode) (caml-in-literal-p)))
	(split-mark))
    (if (not hooked) nil
      (setq split-mark (set-marker (make-marker) (point)))
      (caml-split-string))
    ad-do-it
    (if (not hooked) nil
      (goto-char split-mark)
      (set-marker split-mark nil))))

(defun caml-electric-pipe ()
  "If inserting a | or } operator at beginning of line, reindent the line.

Unfortunately there is a situation where this mechanism gets
confused. It's when | is the first character of a |] sequence. This is
a misfeature of caml syntax and cannot be fixed, however, as a
workaround, the electric ] inserts | itself if the matching [ is
followed by |."
  
  (interactive "*")
  (let ((electric (and caml-electric-indent
		       (caml-in-indentation)
		       (not (caml-in-comment-p)))))
    (self-insert-command 1)
    (if electric
	(let ((indent
	       (save-excursion
		 (backward-char 1)
		 (caml-indent-command)
		 (current-column))))
	  (indent-to (- indent
			(symbol-value
			 (nth 1 (assoc
				 (char-to-string last-command-char)
				 caml-leading-kwops-alist)))))))))

(defun caml-electric-rb ()
  "If inserting a ] operator at beginning of line, reindent the line.

Also, if the matching [ is followed by a | and this ] is not preceded
by |, insert one."

  (interactive "*")
  (let* ((prec (preceding-char))
	 (look-pipe (and caml-electric-close-vector
			 (not (caml-in-comment-p))
			 (not (caml-in-literal-p))
			 (or (not (numberp prec))
			     (not (char-equal ?| prec)))
			 (set-marker (make-marker) (point))))
	 (electric (and caml-electric-indent
			(caml-in-indentation)
			(not (caml-in-comment-p)))))
    (self-insert-command 1)
    (if electric
	(let ((indent
	       (save-excursion
		 (backward-char 1)
		 (caml-indent-command)
		 (current-column))))
	  (indent-to (- indent
			(symbol-value
			 (nth 1 (assoc
				 (char-to-string last-command-char)
				 caml-leading-kwops-alist)))))))
    (if look-pipe
	(save-excursion
	  (let ((insert-pipe
		 (condition-case nil
		     (prog2
		       (backward-list 1)
		       (if (looking-at "\\[|") "|" ""))
		   (error ""))))
	    (goto-char look-pipe)
	    (insert insert-pipe))
	  (set-marker look-pipe nil)))))		 

(defun caml-abbrev-hook ()
  "If inserting a leading keyword at beginning of line, reindent the line."
  ;itz unfortunately we need a special case 
  (if (and (not (caml-in-comment-p)) (not (= last-command-char ?_)))
      (let* ((bol (save-excursion (beginning-of-line) (point)))
	     (kw (save-excursion
		   (and (re-search-backward "^[ \t]*\\(\\sw+\\)\\=" bol t)
			(caml-match-string 1)))))
	(if kw
	    (let ((indent (save-excursion
			    (goto-char (match-beginning 1))
			    (caml-indent-command)
			    (current-column)))
		  (abbrev-correct (if (= last-command-char ?\ ) 1 0)))
	      (indent-to (- indent
			    (symbol-value
			     (nth 1 (assoc kw caml-leading-kwops-alist)))
			    abbrev-correct)))))))

(defun caml-indent-phrase ()
  (interactive "*")
  (let ((bounds (caml-mark-phrase)))
    (indent-region (car bounds) (cdr bounds) nil)))

(defun caml-backward-to-less-indent (&optional n)
  "Move cursor back  N lines with less or same indentation."
  (interactive "p")
  (beginning-of-line 1)
  (if (< n 0) (caml-forward-to-less-indent (- n))
    (while (> n 0)
      (let ((i (current-indentation)))
	(forward-line -1)
	(while (or (> (current-indentation) i)
		   (caml-in-comment-p)
		   (looking-at
		    (concat "[ \t]*\\(\n\\|" comment-start-skip "\\)")))
	  (forward-line -1)))
      (setq n (1- n))))
  (back-to-indentation))

(defun caml-forward-to-less-indent (&optional n)
  "Move cursor back N lines with less or same indentation."
  (interactive "p")
  (beginning-of-line 1)
  (if (< n 0) (caml-backward-to-less-indent (- n))
    (while (> n 0)
      (let ((i (current-indentation)))
	(forward-line 1)
	(while (or (> (current-indentation) i)
		   (caml-in-comment-p)
		   (looking-at
		    (concat "[ \t]*\\(\n\\|" comment-start-skip "\\)")))
	  (forward-line 1)))
      (setq n (1- n))))
  (back-to-indentation))  

(defun caml-insert-begin-form ()
  "Inserts a nicely formatted begin-end form, leaving a mark after end."
  (interactive "*")
  (let ((prec (preceding-char)))
    (if (and (numberp prec) (not (char-equal ?\  (char-syntax prec))))
	(insert " ")))
  (let* ((c (current-indentation)) (i (+ caml-begin-indent c)))
    (insert "begin\n\nend")
    (push-mark)
    (indent-line-to c)
    (forward-line -1)
    (indent-line-to i)))

(defun caml-insert-for-form ()
  "Inserts a nicely formatted for-do-done form, leaving a mark after do(ne)."
  (interactive "*")
  (let ((prec (preceding-char)))
    (if (and (numberp prec) (not (char-equal ?\  (char-syntax prec))))
	(insert " ")))
  (let* ((c (current-indentation)) (i (+ caml-for-indent c)))
    (insert "for  do\n\ndone")
    (push-mark)
    (indent-line-to c)
    (forward-line -1)
    (indent-line-to i)
    (push-mark)
    (beginning-of-line 1)
    (backward-char 4)))
  
(defun caml-insert-if-form ()
  "Insert nicely formatted if-then-else form leaving mark after then, else."
  (interactive "*")
  (let ((prec (preceding-char)))
    (if (and (numberp prec) (not (char-equal ?\  (char-syntax prec))))
	(insert " ")))
  (let* ((c (current-indentation)) (i (+ caml-if-indent c)))
    (insert "if\n\nthen\n\nelse\n")
    (indent-line-to i)
    (push-mark)
    (forward-line -1)
    (indent-line-to c)
    (forward-line -1)
    (indent-line-to i)
    (push-mark)
    (forward-line -1)
    (indent-line-to c)
    (forward-line -1)
    (indent-line-to i)))

(defun caml-insert-match-form ()
  "Insert nicely formatted match-with form leaving mark after with."
  (interactive "*")
  (let ((prec (preceding-char)))
    (if (and (numberp prec) (not (char-equal ?\  (char-syntax prec))))
	(insert " ")))
  (let* ((c (current-indentation)) (i (+ caml-match-indent c)))
    (insert "match\n\nwith\n")
    (indent-line-to i)
    (push-mark)
    (forward-line -1)
    (indent-line-to c)
    (forward-line -1)
    (indent-line-to i)))

(defun caml-insert-let-form ()
  "Insert nicely formatted let-in form leaving mark after in."
  (interactive "*")
  (let ((prec (preceding-char)))
    (if (and (numberp prec) (not (char-equal ?\  (char-syntax prec))))
	(insert " ")))
  (let* ((c (current-indentation)))
    (insert "let  in\n")
    (indent-line-to c)
    (push-mark)
    (forward-line -1)
    (forward-char (+ c 4))))

(defun caml-insert-try-form ()
  "Insert nicely formatted try-with form leaving mark after with."
  (interactive "*")
  (let ((prec (preceding-char)))
    (if (and (numberp prec) (not (char-equal ?\  (char-syntax prec))))
	(insert " ")))
  (let* ((c (current-indentation)) (i (+ caml-try-indent c)))
    (insert "try\n\nwith\n")
    (indent-line-to i)
    (push-mark)
    (forward-line -1)
    (indent-line-to c)
    (forward-line -1)
    (indent-line-to i)))

(defun caml-insert-while-form ()
  "Insert nicely formatted while-do-done form leaving mark after do, done."
  (interactive "*")
  (let ((prec (preceding-char)))
    (if (and (numberp prec) (not (char-equal ?\  (char-syntax prec))))
	(insert " ")))
  (let* ((c (current-indentation)) (i (+ caml-if-indent c)))
    (insert "while  do\n\ndone")
    (push-mark)
    (indent-line-to c)
    (forward-line -1)
    (indent-line-to i)
    (push-mark)
    (beginning-of-line 1)
    (backward-char 4)))

;;; caml.el ends here

(provide 'caml)
