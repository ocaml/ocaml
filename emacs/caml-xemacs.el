(require 'overlay)

;; for caml-help.el
(defun caml-info-other-window (arg)
  (save-excursion (info arg))
  (view-buffer-other-window "*info*"))

;; for caml-types.el
(defun caml-line-beginning-position ()
  (save-excursion (beginning-of-line) (point)))

(defun caml-event-window (e) (event-window e))
(defun caml-event-point-start (e) (event-closest-point e))
(defun caml-event-point-end (e) (event-closest-point e))
(defmacro caml-track-mouse (el) (progn el))
(defalias 'caml-read-event 'next-event)
(defun mouse-movement-p (e) (equal (event-type e) 'motion))

(provide 'caml-xemacs)
