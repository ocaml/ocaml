(require 'overlay)

;; for caml-help.el
(defun info-other-window (arg)
  (save-excursion (info arg))
  (view-buffer-other-window "*info*"))

;; for caml-types.el
(defun event-start (e) e)
(defun event-end (e) e)
(defun line-beginning-position ()
  (save-excursion (beginning-of-line) (point)))
(defvar last-mouse-position t)
(defun posn-point (e) (event-closest-point e))
(defmacro track-mouse (el) (progn el))
(defun read-event () (let ((e (next-event))) e))
(defun mouse-movement-p (e) (equal (event-type e) 'motion))
(defun posn-window (e) (event-window e))

(provide 'caml-xemacs)
