;; for caml-help.el
(defalias 'caml-info-other-window 'info-other-window)

;; for caml-types.el

(defalias 'caml-line-beginning-position 'line-beginning-position)

(defun caml-event-window (e) (posn-window (event-start e)))
(defun caml-event-point-start (e) (posn-point (event-start e)))
(defun caml-event-point-end (e) (posn-point (event-end e)))
(defalias 'caml-read-event 'read-event)
(defmacro caml-track-mouse (&rest body) (cons 'track-mouse body))

(provide 'caml-emacs)
