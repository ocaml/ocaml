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


; An emacs-lisp complement to the "-dtypes" option of ocamlc and ocamlopt.

; Format of the *.annot files:

; file ::= block *
; block ::= position <SP> position <LF> annotation *
; position ::= filename <SP> num <SP> num <SP> num
; annotation ::= keyword open-paren <LF> <SP> <SP> data <LF> close-paren

; <SP> is a space character (ASCII 0x20)
; <LF> is a line-feed character (ASCII 0x0A)
; num is a sequence of decimal digits
; filename is a string with the lexical conventions of O'Caml
; open-paren is an open parenthesis (ASCII 0x28)
; close-paren is a closed parenthesis (ASCII 0x29)
; data is any sequence of characters where <LF> is always followed by
;      at least two space characters.

; in each block, the two positions are respectively the start and the
; end of the range described by the block.
; in a position, the filename is the name of the file, the first num
; is the line number, the second num is the offset of the beginning
; of the line, the third num is the offset of the position itself.
; the char number within the line is the difference between the third
; and second nums.

; For the moment, the only possible keyword is "type".


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
  "Show the type of expression or pattern at point.
   The smallest expression or pattern that contains point is
   temporarily highlighted.  Its type is highlighted in the .annot
   file and the mark is set to the beginning of the type.
   The type is also displayed in the mini-buffer.

   Hints on using the type display:
   . If you want the type of an identifier, put point within any
     occurrence of this identifier.
   . If you want the result type of a function application, put point
     at the first space after the function name.
   . If you want the type of a list, put point on a bracket, on a
     semicolon, or on the :: constructor.
   . Even if type checking fails, you can still look at the types
     in the file, up to where the type checker failed."
  (interactive)
  (let* ((target-buf (current-buffer))
         (target-file (file-name-nondirectory (buffer-file-name)))
         (target-date (nth 5 (file-attributes (buffer-file-name))))
         (target-line (1+ (count-lines (point-min) (line-beginning-position))))
         (target-bol (line-beginning-position))
         (target-cnum (point))
         (type-file (concat (file-name-sans-extension (buffer-file-name))
                            ".annot"))
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
                (delete-overlay caml-types-expr-ovl)
                (delete-overlay caml-types-type-ovl)
                (message "Point is not within a typechecked expression or pattern."))
            (let ((left (caml-types-get-pos target-buf (nth 0 loc) (nth 1 loc)))
                  (right (caml-types-get-pos target-buf
                                             (nth 2 loc) (nth 3 loc))))
              (move-overlay caml-types-expr-ovl left right target-buf))
            (re-search-forward "^type(");; not strictly correct
            (forward-line 1)
            (re-search-forward "  \\(\\([^\n)]\\|.)\\|\n[^)]\\)*\\)\n)")
            (move-overlay caml-types-type-ovl (match-beginning 1) (match-end 1)
                          type-buf)
            (message (format "type: %s" (match-string 1)))
            (set-mark (match-beginning 1)))))
      (let
          ((window (get-buffer-window type-buf))
           (this-window (selected-window)))
        (if window
            (progn
              (select-window window)
              (goto-char (mark))
              (select-window this-window))))
      (unwind-protect
          (sit-for 60)
        (delete-overlay caml-types-expr-ovl)))))

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


;; bindings

(and
 (boundp 'caml-mode-map)
 (keymapp caml-mode-map)
 (progn 
   (define-key caml-mode-map [?\C-c?\C-t] 'caml-types-show-type)
   (let ((map (lookup-key caml-mode-map [menu-bar caml])))
     (and
      (keymapp map)
      (progn
        (define-key map [separator-types] '("---"))
        (define-key map [show-type]
          '("Show type at point" . caml-types-show-type )))))))

(provide 'caml-types)
