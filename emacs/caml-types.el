;(***********************************************************************)
;(*                                                                     *)
;(*                           Objective Caml                            *)
;(*                                                                     *)
;(*          Damien Doligez, projet Moscova, INRIA Rocquencourt         *)
;(*                                                                     *)
;(*  Copyright 2003 Institut National de Recherche en Informatique et   *)
;(*  en Automatique.  All rights reserved.  This file is distributed    *)
;(*  under the terms of the Q Public License version 1.0.               *)
;(*                                                                     *)
;(***********************************************************************)

;(* $Id$ *)

; WARNING:
; This code is experimental.  Everything may change at any time.

; An emacs-lisp complement to the "-stypes" option of ocamlc and ocamlopt.
; Load this file in your emacs, then C-c C-t will show you the
; type of the expression that contains the cursor.
; The expression is highlighted in the current buffer.
; The type is highlighted in "foo.types" (if your file is "foo.ml"),
; which is convenient if the type doesn't fit on a line.
; (doesn't work very well).


; Hints on using the type display:

; . If you want the type of an identifier, put the cursor in any
;   occurrence of this identifier (as expression or as pattern) and
;   type C-c C-t
; . If you want the result type of a function application, put the
;   cursor at the first space after the function name
; . If you want the type of a parenthesized expression, put the
;   cursor at the first space after the open parenthesis
; . If you want the type of a list, put the cursor on a bracket,
;   or on a semicolon, or on the :: constructor
; . Even if type checking fails, you can still look at the types
;   in the file, up to and including where the type checker failed.



; TO DO:
; - make it work with camlp4-processed files
; - make emacs scroll the .types file when we move the point there,
;   even if the file is already displayed
; - (?) integrate this file into caml.el
; - (?) use dichotomy to find the location faster in the .types file

      
; (global-set-key "\C-c\C-t" 'caml-types-show-type)


(setq caml-types-filename-re "\"\\(\\([^\\\"]\\|\\\\.\\)*\\)\"")
(setq caml-types-number-re "\\([0-9]*\\)")
(setq caml-types-position-re
      (concat caml-types-filename-re " "
              caml-types-number-re " "
              caml-types-number-re " "
              caml-types-number-re))
(setq caml-types-location-re
      (concat "^" caml-types-position-re " " caml-types-position-re))
(setq caml-types-expr-ov (make-overlay 1 1))
(overlay-put caml-types-expr-ov 'face 'region)
(setq caml-types-type-ov (make-overlay 1 1))
(overlay-put caml-types-type-ov 'face 'region)
(setq caml-types-not-found-msg
      "The cursor is not within a typechecked expression or pattern.")

(defun caml-types-show-type ()
  "Highlight the smallest expression that contains the cursor,
   and display its type in the minibuffer."
  (interactive)
  (let* (type-point
         (target-file (file-name-nondirectory (buffer-file-name)))
         (target-date (nth 5 (file-attributes (buffer-file-name))))
         (target-line (1+ (count-lines (point-min) (line-beginning-position))))
         (target-col (- (point) (line-beginning-position)))
         (target-buf (current-buffer))
         (type-file (concat (file-name-sans-extension (buffer-file-name))
                            ".types"))
         (type-date (nth 5 (file-attributes type-file)))
         (type-buf (find-file-noselect type-file)))
    (if (caml-types-date< type-date target-date)
        (message (format "%s is more recent than %s" target-file type-file))
      (save-excursion
        (set-buffer type-buf)
        (goto-char (point-min))
        (let ((loc (caml-types-find-location target-file target-line
                                             target-col)))
          (if (null loc)
              (progn
                (move-overlay caml-types-expr-ov 1 1)
                (move-overlay caml-types-type-ov 1 1)
                (message caml-types-not-found-msg))
            (let ((left (caml-types-get-pos target-buf (car loc)))
                  (right (caml-types-get-pos target-buf (cdr loc))))
              (move-overlay caml-types-expr-ov left right target-buf))
            (forward-line 2)
            (re-search-forward "  \\(\\([^\n)]\\|.)\\|\n[^)]\\)*\\)\n)")
            (move-overlay caml-types-type-ov (match-beginning 1) (match-end 1)
                          type-buf)
            (message (format "type: %s" (match-string 1)))
            (setq type-point (match-beginning 1)))))
      (if (null type-point)
          ()
        (set-buffer type-buf)
        (goto-char type-point)
        (set-buffer target-buf)))))

(defun caml-types-date< (date1 date2)
  (or (< (car date1) (car date2))
      (and (= (car date1) (car date2))
           (< (nth 1 date1) (nth 1 date2)))))

(defun caml-types-pair=< (p1 p2)
  (or (< (car p1) (car p2))
      (and (= (car p1) (car p2))
           (<= (cdr p1) (cdr p2)))))

(defun caml-types-find-location (target-file target-line target-col)
  (let (left-file left-line left-bol left-char
        right-file right-line right-bol right-char
        found)
    (catch 'exit
      (while (re-search-forward caml-types-location-re () t)
        (setq left-file (file-name-nondirectory (match-string 1)))
        (setq left-line (string-to-int (match-string 3)))
        (setq left-bol (string-to-int (match-string 4)))
        (setq left-cnum (string-to-int (match-string 5)))
        (setq right-file (file-name-nondirectory (match-string 6)))
        (setq right-line (string-to-int (match-string 8)))
        (setq right-bol (string-to-int (match-string 9)))
        (setq right-cnum (string-to-int (match-string 10)))
        (let ((left-col (- left-cnum left-bol))
              (right-col (- right-cnum right-bol))
              (target-line-col (cons target-line target-col)))
          (if (and (string= left-file target-file)
                   (string= right-file target-file)
                   (caml-types-pair=< (cons left-line left-col) target-line-col)
                   (not (caml-types-pair=< (cons right-line right-col)
                                           target-line-col)))
              (throw 'exit (cons (cons left-line left-col)
                                 (cons right-line right-col))))))
      ())))

(defun caml-types-get-pos (buf pos)
  (save-excursion
    (set-buffer buf)
    (goto-line (car pos))
    (forward-char (cdr pos))
    (point)))
