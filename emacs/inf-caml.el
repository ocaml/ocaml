;;; inf-caml.el --- run the Caml toplevel in an Emacs buffer

;; Xavier Leroy, july 1993.

(require 'comint)
(require 'caml)

(defvar inferior-caml-mode-map nil)
(if inferior-caml-mode-map nil
  (setq inferior-caml-mode-map
        (copy-keymap comint-mode-map)))

;; Augment Caml mode, so you can process Caml code in the source files.

(define-key caml-mode-map "\M-\C-x" 'caml-eval-phrase)
(define-key caml-mode-map "\C-x\C-e" 'caml-eval-phrase)
(define-key caml-mode-map "\C-c\C-e" 'caml-eval-phrase)
(define-key caml-mode-map "\C-c\C-r" 'caml-eval-region)

(defvar inferior-caml-program "ocaml"
  "*Program name for invoking an inferior Caml from Emacs.")

(defun inferior-caml-mode ()
  "Major mode for interacting with an inferior Caml process.
Runs a Caml toplevel as a subprocess of Emacs, with I/O through an
Emacs buffer. A history of input phrases is maintained. Phrases can
be sent from another buffer in Caml mode.

\\{inferior-caml-mode-map}"
  (interactive)
  (comint-mode)
  (setq comint-prompt-regexp "^# ?")
  (setq major-mode 'inferior-caml-mode)
  (setq mode-name "Inferior Caml")
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
  (use-local-map inferior-caml-mode-map)
  (run-hooks 'inferior-caml-mode-hooks))

(defun run-caml (cmd)
  "Run an inferior Caml process.
Input and output via buffer `*inferior-caml*'."
  (interactive (list (read-from-minibuffer "Caml command to run: "
                                           inferior-caml-program)))
  (setq inferior-caml-program cmd)
  (if (not (comint-check-proc "*inferior-caml*"))
      (let ((cmdlist (inferior-caml-args-to-list cmd))
            (process-connection-type nil))
	(set-buffer (apply (function make-comint)
			   "inferior-caml" (car cmdlist) nil (cdr cmdlist)))
	(inferior-caml-mode)))
  (switch-to-buffer "*inferior-caml*"))

(defun inferior-caml-args-to-list (string)
  (let ((where (string-match "[ \t]" string)))
    (cond ((null where) (list string))
	  ((not (= where 0))
	   (cons (substring string 0 where)
		 (inferior-caml-args-to-list (substring string (+ 1 where)
							(length string)))))
	  (t (let ((pos (string-match "[^ \t]" string)))
	       (if (null pos)
		   nil
		 (inferior-caml-args-to-list (substring string pos
							(length string)))))))))

(defun caml-eval-region (start end)
  "Send the current region to the inferior Caml process."
  (interactive"r")
  (comint-send-region "*inferior-caml*" start end)
  (comint-send-string "*inferior-caml*" ";;\n")
  (if (not (get-buffer-window "*inferior-caml*" t))
      (display-buffer "*inferior-caml*")))

(defun caml-eval-phrase ()
  "Send the current Caml phrase to the inferior Caml process."
  (interactive)
  (save-excursion
    (let ((bounds (caml-mark-phrase)))
    (caml-eval-region (car bounds) (cdr bounds)))))

;;; inf-caml.el ends here

(provide 'inf-caml)
