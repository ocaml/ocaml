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

; An emacs-lisp complement to the "-dtypes" option of ocamlc and ocamlopt.
; Load this file in your emacs, then C-c C-t will show you the
; type of the expression (or pattern) that contains the cursor.
; The expression is highlighted in the current buffer.
; The type is highlighted in "foo.types" (if your file is "foo.ml"),
; which is convenient when the type doesn't fit on a line.


; Hints on using the type display:

; . If you want the type of an identifier, put the cursor in any
;   occurrence of this identifier (as expression or as pattern) and
;   type C-c C-t
; . If you want the result type of a function application, put the
;   cursor at the first space after the function name
; . If you want the type of a list, put the cursor on a bracket,
;   or on a semicolon, or on the :: constructor
; . Even if type checking fails, you can still look at the types
;   in the file, up to where the type checker failed.
; . To get rid of the highlighting, put the cursor in a comment
;   and type C-c C-t.
; . The mark in the .types file is set to the beginning of the
;   type, so you can type C-x C-x in that file to view the type.



; TO DO:
; - make emacs scroll the .types file to show the type
; - (?) integrate this file into caml.el

      
; (global-set-key "\C-c\C-t" 'caml-types-show-type)


(let* ((caml-types-filename-re "\"\\(\\([^\\\"]\\|\\\\.\\)*\\)\"")
       (caml-types-number-re "\\([0-9]*\\)")
       (caml-types-position-re
        (concat caml-types-filename-re " "
                caml-types-number-re " "
                caml-types-number-re " "
                caml-types-number-re)))
  (setq caml-types-location-re
        (concat "^" caml-types-position-re " " caml-types-position-re)))

(setq caml-types-expr-ovl (make-overlay 1 1))
(overlay-put caml-types-expr-ovl 'face 'region)
(setq caml-types-type-ovl (make-overlay 1 1))
(overlay-put caml-types-type-ovl 'face 'region)

(defun caml-types-show-type ()
  "Highlight the smallest expression that contains the cursor,
   and display its type in the minibuffer."
  (interactive)
  (let* ((target-buf (current-buffer))
         (target-file (file-name-nondirectory (buffer-file-name)))
         (target-date (nth 5 (file-attributes (buffer-file-name))))
         (target-line (1+ (count-lines (point-min) (line-beginning-position))))
         (target-bol (line-beginning-position))
         (target-cnum (point))
         (type-file (concat (file-name-sans-extension (buffer-file-name))
                            ".types"))
         (type-date (nth 5 (file-attributes type-file)))
         (type-buf (caml-types-find-file type-file)))
    (if (caml-types-date< type-date target-date)
        (message (format "%s is more recent than %s" target-file type-file))
      (save-excursion
        (set-buffer type-buf)
        (goto-char (point-min))
        (let ((loc (caml-types-find-location target-file target-line
                                             target-bol target-cnum)))
          (if (null loc)
              (progn
                (move-overlay caml-types-expr-ovl 1 1)
                (move-overlay caml-types-type-ovl 1 1)
                (message "The cursor is not within a typechecked expression or pattern."))
            (let ((left (caml-types-get-pos target-buf (nth 0 loc) (nth 1 loc)))
                  (right (caml-types-get-pos target-buf
                                             (nth 2 loc) (nth 3 loc))))
              (move-overlay caml-types-expr-ovl left right target-buf))
            (forward-line 2)
            (re-search-forward "  \\(\\([^\n)]\\|.)\\|\n[^)]\\)*\\)\n)")
            (move-overlay caml-types-type-ovl (match-beginning 1) (match-end 1)
                          type-buf)
            (message (format "type: %s" (match-string 1)))
            ; *** this doesn't seem to work, I don't know why...
            ; *** (goto-char type-point)
            ; *** workaround: set the mark instead
            (set-mark (match-beginning 1))
            (set-buffer target-buf)))))))

(defun caml-types-date< (date1 date2)
  (or (< (car date1) (car date2))
      (and (= (car date1) (car date2))
           (< (nth 1 date1) (nth 1 date2)))))

(defun caml-types-find-location (targ-file targ-line targ-bol targ-cnum)
  (let (found)
    (catch 'exit
      (while (re-search-forward caml-types-location-re () t)
        (let ((left-file (file-name-nondirectory (match-string 1)))
              (left-line (string-to-int (match-string 3)))
              (left-bol (string-to-int (match-string 4)))
              (left-cnum (string-to-int (match-string 5)))
              (right-file (file-name-nondirectory (match-string 6)))
              (right-line (string-to-int (match-string 8)))
              (right-bol (string-to-int (match-string 9)))
              (right-cnum (string-to-int (match-string 10))))
          (if (and (caml-types-pos<= left-file left-line left-bol left-cnum
                                     targ-file targ-line targ-bol targ-cnum)
                   (caml-types-pos> right-file right-line right-bol right-cnum
                                    targ-file targ-line targ-bol targ-cnum))
              (throw 'exit (list left-line (- left-cnum left-bol)
                                 right-line (- right-cnum right-bol)))))))))


;; Warning: these comparison functions are not symmetric.
;; The first argument determines the format:
;; when its file component is empty, only the cnum is compared.

(defun caml-types-pos<= (file1 line1 bol1 cnum1 file2 line2 bol2 cnum2)
  (if (string= file1 "")
      (<= cnum1 cnum2)
    (and (string= file1 file2)
         (or (< line1 line2)
             (and (= line1 line2)
                  (<= (- cnum1 bol1) (- cnum2 bol2)))))))

(defun caml-types-pos> (file1 line1 bol1 cnum1 file2 line2 bol2 cnum2)
  (if (string= file1 "")
      (> cnum1 cnum2)
    (and (string= file1 file2)
         (or (> line1 line2)
             (and (= line1 line2)
                  (> (- cnum1 bol1) (- cnum2 bol2)))))))

(defun caml-types-get-pos (buf line col)
  (save-excursion
    (set-buffer buf)
    (goto-line line)
    (forward-char col)
    (point)))

; find-file-read-only-noselect seems to be missing from emacs...
(defun caml-types-find-file (name)
  (or (and (get-file-buffer name)
           (find-file-noselect name))
      (let ((buf (find-file-noselect name)))
        (save-excursion
          (set-buffer buf)
          (toggle-read-only 1))
        buf)))
