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


(defvar caml-types-location-re nil "Regexp to parse *.annot files.

Annotation files *.annot may be generated with the \"-dtypes\" option 
of ocamlc and ocamlopt. 

Their format is:

  file ::= block *
  block ::= position <SP> position <LF> annotation *
  position ::= filename <SP> num <SP> num <SP> num
  annotation ::= keyword open-paren <LF> <SP> <SP> data <LF> close-paren

  <SP> is a space character (ASCII 0x20)
  <LF> is a line-feed character (ASCII 0x0A)
  num is a sequence of decimal digits
  filename is a string with the lexical conventions of O'Caml
  open-paren is an open parenthesis (ASCII 0x28)
  close-paren is a closed parenthesis (ASCII 0x29)
  data is any sequence of characters where <LF> is always followed by
       at least two space characters.

- in each block, the two positions are respectively the start and the
- end of the range described by the block.
- in a position, the filename is the name of the file, the first num
  is the line number, the second num is the offset of the beginning
  of the line, the third num is the offset of the position itself.
- the char number within the line is the difference between the third
  and second nums.

For the moment, the only possible keyword is \"type\"."
)

(let* ((caml-types-filename-re "\"\\(\\([^\\\"]\\|\\\\.\\)*\\)\"")
       (caml-types-number-re "\\([0-9]*\\)")
       (caml-types-position-re
        (concat caml-types-filename-re " "
                caml-types-number-re " "
                caml-types-number-re " "
                caml-types-number-re)))
  (setq caml-types-location-re
        (concat "^" caml-types-position-re " " caml-types-position-re)))

(defvar caml-types-expr-ovl (make-overlay 1 1))

(make-face 'caml-types-face)
(set-face-doc-string 'caml-types-face
                     "face for hilighting expressions and types")
(if (not (face-differs-from-default-p 'caml-types-face))
    (set-face-background 'caml-types-face "#88FF44"))

(overlay-put caml-types-expr-ovl 'face 'caml-types-face)


(defvar caml-types-annotation-tree)
(defvar caml-types-annotation-date)
(make-variable-buffer-local 'caml-types-annotation-tree)
(make-variable-buffer-local 'caml-types-annotation-date)
(set-default 'caml-types-annotation-tree ())


(defun caml-types-show-type (arg)
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
     in the file, up to where the type checker failed.

See also `caml-types-explore' for exploration by mouse dragging.
See `caml-types-location-re' for annotation file format.
"
  (interactive "p")
  (let* ((target-buf (current-buffer))
         (target-file (file-name-nondirectory (buffer-file-name)))
         (target-line (1+ (count-lines (point-min) (line-beginning-position))))
         (target-bol (line-beginning-position))
         (target-cnum (point))
         (type-file (concat (file-name-sans-extension (buffer-file-name))
                            ".annot"))
         (type-buf (caml-types-find-file type-file)))
    (caml-types-preprocess type-file type-buf)
    (let* ((targ-loc (vector target-file target-line target-bol target-cnum))
           (node (caml-types-find-location targ-loc ()
                                           caml-types-annotation-tree)))
      (cond
       ((null node)
        (delete-overlay caml-types-expr-ovl)
        (message "Point is not within a typechecked expression or pattern.")
        (with-current-buffer type-buf (narrow-to-region 1 1)))
       (t
        (let ((left (caml-types-get-pos target-buf (elt node 0)))
              (right (caml-types-get-pos target-buf (elt node 1)))
              (ty-start (car (elt node 2)))
              (ty-end (cdr (elt node 2))))
          (move-overlay caml-types-expr-ovl left right target-buf)
          (with-current-buffer type-buf
            (widen)
            (message (format "type: %s" (buffer-substring ty-start
                                                          (1- ty-end))))
            (narrow-to-region ty-start ty-end))))))
    (if (and (= arg 4)
             (not (window-live-p (get-buffer-window type-buf))))
        (display-buffer type-buf))
    (unwind-protect
        (sit-for 60)
      (delete-overlay caml-types-expr-ovl))))

(defun caml-types-preprocess (type-file type-buf)
  (let ((type-date (nth 5 (file-attributes type-file)))
        (target-date (nth 5 (file-attributes (buffer-file-name)))))
    (unless (and caml-types-annotation-tree
                 (not (caml-types-date< caml-types-annotation-date type-date)))
      (if (caml-types-date< type-date target-date)
          (error (format "%s is more recent than %s" target-file type-file)))
      (message "reading annotation file...")
      (let ((tree (with-current-buffer type-buf
                    (widen)
                    (goto-char (point-min))
                    (caml-types-build-tree))))
        (setq caml-types-annotation-tree tree
              caml-types-annotation-date type-date)
        (message "")))))

(defun caml-types-date< (date1 date2)
  (or (< (car date1) (car date2))
      (and (= (car date1) (car date2))
           (< (nth 1 date1) (nth 1 date2)))))

; tree of intervals
; each node is a vector
; [ pos-left pos-right type-info child child child... ]
; type-info =
;  () if this node does not correspond to an annotated interval
;  (type-start . type-end)  address of the annotation in the .annot file

(defun caml-types-build-tree ()
  (let ((stack ())
        (accu ())
        (type-info ()))
    (while (re-search-forward caml-types-location-re () t)
      (let ((l-file (file-name-nondirectory (match-string 1)))
            (l-line (string-to-int (match-string 3)))
            (l-bol (string-to-int (match-string 4)))
            (l-cnum (string-to-int (match-string 5)))
            (r-file (file-name-nondirectory (match-string 6)))
            (r-line (string-to-int (match-string 8)))
            (r-bol (string-to-int (match-string 9)))
            (r-cnum (string-to-int (match-string 10))))
        (while (and (re-search-forward "^" () t)
                    (not (looking-at "type"))
                    (not (looking-at "\\\"")))
          (forward-char 1))
        (setq type-info
              (if (looking-at "^type(\n\\(  \\([^\n)]\\|.)\\|\n[^)]\\)*\n\\))")
                  (cons (match-beginning 1) (match-end 1))))
        (setq accu ())
        (while (and stack
                    (caml-types-pos-contains l-cnum r-cnum (car stack)))
          (setq accu (cons (car stack) accu))
          (setq stack (cdr stack)))
        (let* ((left-pos (vector l-file l-line l-bol l-cnum))
               (right-pos (vector r-file r-line r-bol r-cnum))
               (node (caml-types-make-node left-pos right-pos type-info accu)))
          (setq stack (cons node stack)))))
    (if (null stack)
        (vector)
      (let* ((left-pos (elt (car (last stack)) 0))
             (right-pos (elt (car stack) 1)))
        (if (null (cdr stack))
            (car stack)
          (caml-types-make-node left-pos right-pos () (nreverse stack)))))))

(defun caml-types-make-node (left-pos right-pos type-info children)
  (let ((result (make-vector (+ 3 (length children)) ()))
        (i 3))
    (aset result 0 left-pos)
    (aset result 1 right-pos)
    (aset result 2 type-info)
    (while children
      (aset result i (car children))
      (setq children (cdr children))
      (setq i (1+ i)))
    result))

(defun caml-types-pos-contains (l-cnum r-cnum node)
  (and (<= l-cnum (elt (elt node 0) 3))
       (>= r-cnum (elt (elt node 1) 3))))

(defun caml-types-find-location (targ-pos curr node)
  (let ((i 3))
    (if (not (caml-types-pos-inside targ-pos node))
        curr
      (while (and (< i (length node))
                  (not (caml-types-pos-inside targ-pos (elt node i))))
        (setq i (1+ i)))
      (if (elt node 2)
          (setq curr node))
      (if (< i (length node))
          (caml-types-find-location targ-pos curr (elt node i))
        curr))))

(defun caml-types-pos-inside (pos node)
  (let ((left-pos (elt node 0))
        (right-pos (elt node 1)))
    (and (caml-types-pos<= left-pos pos)
         (caml-types-pos> right-pos pos))))

(defun caml-types-find-interval (buf targ-pos node)
  (let ((nleft (elt node 0))
        (nright (elt node 1))
        (left ())
        (right ())
        (i 3))
    (cond
     ((not (caml-types-pos-inside targ-pos node))
      (if (not (caml-types-pos<= nleft targ-pos))
          (setq right nleft))
      (if (not (caml-types-pos> nright targ-pos))
          (setq left nright)))
     (t
      (setq left nleft
            right nright)
      (while (and (< i (length node))
                  (caml-types-pos<= (elt (elt node i) 0) targ-pos))
        (setq i (1+ i)))
      (if (< i (length node))
          (setq right (elt (elt node i) 0)))
      (if (> i 3)
          (setq left (elt (elt node (1- i)) 1)))))
     (cons (if left
               (caml-types-get-pos buf left)
             (with-current-buffer buf (point-min)))
           (if right
               (caml-types-get-pos buf right)
             (with-current-buffer buf (point-max))))))


;; Warning: these comparison functions are not symmetric.
;; The first argument determines the format:
;; when its file component is empty, only the cnum is compared.

(defun caml-types-pos<= (pos1 pos2)
  (let ((file1 (elt pos1 0))
        (line1 (elt pos1 1))
        (bol1 (elt pos1 2))
        (cnum1 (elt pos1 3))
        (file2 (elt pos2 0))
        (line2 (elt pos2 1))
        (bol2 (elt pos2 2))
        (cnum2 (elt pos2 3)))
    (if (string= file1 "")
        (<= cnum1 cnum2)
      (and (string= file1 file2)
           (or (< line1 line2)
               (and (= line1 line2)
                    (<= (- cnum1 bol1) (- cnum2 bol2))))))))

(defun caml-types-pos> (pos1 pos2)
  (let ((file1 (elt pos1 0))
        (line1 (elt pos1 1))
        (bol1 (elt pos1 2))
        (cnum1 (elt pos1 3))
        (file2 (elt pos2 0))
        (line2 (elt pos2 1))
        (bol2 (elt pos2 2))
        (cnum2 (elt pos2 3)))
    (if (string= file1 "")
        (> cnum1 cnum2)
      (and (string= file1 file2)
           (or (> line1 line2)
               (and (= line1 line2)
                    (> (- cnum1 bol1) (- cnum2 bol2))))))))

(defun caml-types-get-pos (buf pos)
  (save-excursion
    (set-buffer buf)
    (goto-line (elt pos 1))
    (forward-char (- (elt pos 3) (elt pos 2)))
    (point)))

; find-file-read-only-noselect seems to be missing from emacs...
(defun caml-types-find-file (name)
  (let (buf)
  (cond
   ((setq buf (get-file-buffer name))
    (unless (verify-visited-file-modtime buf)
      (if (buffer-modified-p buf)
          (find-file-noselect name)
        (with-current-buffer buf (revert-buffer t t)))
      ))
   ((and (file-readable-p name)
         (setq buf (find-file-noselect name)))
     (with-current-buffer buf (toggle-read-only 1))
     )
   (t
    (error "No annotation file. You may compile with \"-dtypes\" option"))
    )
  buf))

(defun caml-types-explore (event)
  "Explore type annotations by mouse dragging.

The expression under the mouse is highlighted
and its type is displayed in the minibuffer, until the move is released."
  (interactive "e")
  (set-buffer (window-buffer (posn-window (event-start event))))
  (let* ((target-buf (current-buffer))
         (target-file (file-name-nondirectory (buffer-file-name)))
         (type-file (concat (file-name-sans-extension (buffer-file-name))
                            ".annot"))
         (type-buf (caml-types-find-file type-file))
         (target-line) (target-bol)
         target-pos
         Left Right limits cnum loc mes ty-start ty-end
         (tree caml-types-annotation-tree)
         )
    (caml-types-preprocess type-file type-buf)
    ; (message "Drag the mouse to explore types")
    (unwind-protect
        (track-mouse
          (while (and event
                      (integer-or-marker-p
                       (setq cnum (posn-point (event-end event)))))
            (if (and limits (>= cnum (car limits)) (< cnum (cdr limits)))
                (message mes)
              (setq target-bol
                    (save-excursion (goto-char cnum) (line-beginning-position)))
              (setq target-line
                    (1+ (count-lines (point-min) target-bol)))
              (setq target-pos (vector target-file target-line target-bol cnum))
              (save-excursion
                (set-buffer type-buf)
                (widen)
                (setq node (caml-types-find-location target-pos () tree))
                (cond
                 (node
                  (setq Left (caml-types-get-pos target-buf (elt node 0)))
                  (setq Right (caml-types-get-pos target-buf (elt node 1)))
                  (move-overlay caml-types-expr-ovl Left Right target-buf)
                  (setq limits (caml-types-find-interval target-buf target-pos
                                                         node))
                  (setq ty-start (car (elt node 2))
                        ty-end (cdr (elt node 2)))
                  (setq mes (format "type: %s" (buffer-substring ty-start
                                                                 (1- ty-end))))
                  (narrow-to-region ty-start ty-end)
                  )
                 (t
                  (delete-overlay caml-types-expr-ovl)
                  (setq mes nil)
                  (narrow-to-region 1 1)
                  (setq limits (caml-types-find-interval target-buf target-pos
                                                         tree))
                 )))
              (message mes)
              )
              (setq event (read-event))
              (unless (mouse-movement-p event) (setq event nil))
            )
          )
    (delete-overlay caml-types-expr-ovl))
    ))

(provide 'caml-types)
