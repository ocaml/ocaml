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

; TO DO:
; - make emacs scroll the .types file when we move the point there,
;   even if the file is already displayed
; - emit a warning when the .ml file is more recent than the .types
; - make the command work only in caml-mode
; - integrate this file into caml.el (?)
; - write a proper documentation string for caml-types-show-type

(setq caml-types-filename-re "\"\\([^\\\"]\\|\\\\.\\)*\"")
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

(defun caml-types-show-type ()
  "blah"
  (interactive)
  (let* (type-point
         (target (point))
         (targetbuf (current-buffer))
         (typefile (concat (file-name-sans-extension (buffer-file-name))
                           ".types"))
         (tbuf (or (get-file-buffer typefile)
                   (find-file-noselect typefile))))
    (save-excursion
      (set-buffer tbuf)
      (goto-char (point-min))
      (let ((loc (caml-types-find-location target)))
        (if (null loc)
            (progn
              (move-overlay caml-types-expr-ov 1 1)
              (move-overlay caml-types-type-ov 1 1)
              (message "The cursor is not within an expression."))
          (move-overlay caml-types-expr-ov (car loc) (cdr loc) targetbuf)
          (forward-line 2)
          (re-search-forward "  \\(\\([^\n)]\\|.)\\|\n[^)]\\)*\\)\n)")
          (move-overlay caml-types-type-ov (match-beginning 1) (match-end 1)
                        (current-buffer))
          (message (concat "type: " (match-string 1)))
          (setq type-point (match-beginning 1)))))
    (if (null type-point)
        ()
      (set-buffer tbuf)
      (goto-char type-point)
      (set-buffer targetbuf))))

(defun caml-types-find-location (target)
  (let (left right found)
    (catch 'exit
      (while (re-search-forward caml-types-location-re () t)
        (setq left (1+ (string-to-int (match-string 4))))
        (setq right (1+ (string-to-int (match-string 8))))
        (if (and (<= left target) (< target right))
            (throw 'exit (cons left right)))))))

(global-set-key "\C-c\C-t" 'caml-types-show-type)
