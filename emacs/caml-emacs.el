;; for caml-help.el
(defalias 'caml-info-other-window 'info-other-window)

;; for caml-types.el

(defalias 'caml-line-beginning-position 'line-beginning-position)

(defun caml-event-window (e) (event-window (event-start e)))
(defun caml-event-point-start (e) (posn-point (event-stact e)))
(defun caml-event-point-end (e) (posn-point (event-end e)))
(defmacro caml-track-mouse (el) (track-mouse el))
(defalias 'caml-read-event 'read-event)
(defun caml-mouse-movement-p mouse-mouvement)

(provide 'caml-emacs)
