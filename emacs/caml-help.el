;; caml-info.el --- contextual completion and help to caml-mode

;; Didier Remy, November 2001.

;; This provides two functions completion and help
;; look for ocaml-complete and ocaml-help

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  This is a preliminary version.
;;
;;  Possible improvements?
;;   - dump some databaes: Info, Lib, ...
;;   - accept a search path for local libraries instead of current dir
;;     (then distinguish between different modules lying in different
;;     directories) 
;;   - improve the construction for info files.
;;
;;  Abstract over 
;;   - the viewing method and the database, so that the documentation for
;;     and identifier could be search in 
;;       * info / html / man / mli's sources
;;       * viewed in emacs or using an external previewer.
;;
;;  Take all identifiers (labels, Constructors, exceptions, etc.)
;;       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Loading or building databases.
;; 

(require 'info)

;; variables to be customized

(defvar ocaml-lib-path
  '("/usr/lib/ocaml/" "/usr/local/lib/ocaml/")
  "Path for ocaml lib sources (mli files)")

(defvar ocaml-info-alist 'ocaml-info-default-function
  "A-list binding module names to info entries: 

  nil means do not use info.

  A function to build the list lazily (at the first call). The result of
the function call will be assign permanently to this variable for future
uses. We provide a default function \\[ocaml-info-default-function]. 

  Otherwise, this value should be an alist binding module names to info
entries of the form to \"(entry)section\" be taken by the \\[info] 
command. An entry may be an info module or a complete file name."
)


(defvar ocaml-info-name-list "ocaml"
  "Name of ocaml info files describing library modules.")


;; General purpose auxiliary functions

(defun ocaml-capitalize (s)
  (concat (capitalize (substring s 0 1)) (substring s 1)))

(defun ocaml-uncapitalize (s)
  (concat (downcase (substring s 0 1)) (substring s 1)))

(defun iter (f l) (while (consp l) (apply f (list (car l))) (setq l (cdr l))))

(defun ocaml-find-files (path filter &optional depth split)
  (message "%s - %s" path filter)
  (let* ((path-string
          (if (stringp path)
              (if (file-directory-p path) path nil)
            (mapconcat '(lambda (d) (if (file-directory-p d) d))
                       path " "))) 
         (command
          (and path-string
               (concat "find " path-string
                       " '(' " filter " ')' "
                       (if depth (concat " -maxdepth " (int-to-string depth)))
                       (if split nil " -printf '%\p '") 
                       )))
          (files
           (and command (shell-command-to-string command))))
         (if (and split (stringp files)) (split-string files "\n") files) 
         ))

;; Specialized auxiliary functions


;; Global table of modules contents of modules loaded lazily.

(defvar ocaml-module-alist 'lazy
  "A-list of modules with how and where to find help information. 
  'delay means non computed yet")

(defun ocaml-add-mli-modules (modules tag &optional path)
  (let ((files
         (ocaml-find-files (or path ocaml-lib-path)
                           "-type f -name '*.mli'" 1 t)))
    (while (consp files)
      (if (string-match "\\([^/]*\\).mli" (car files))
          (let* ((module (ocaml-capitalize (match-string 1 (car files))))
                 (dir (file-name-directory (car files)))
                 (dirp (member dir ocaml-lib-path)))
            (if (and (consp dirp) (string-equal dir (car dirp)))
                (setq dir (car dirp)))
            (if (assoc module modules) nil
              (setq modules
                    (cons (cons module (cons (cons tag dir) 'lazy)) modules))
              )))
      (setq files (cdr files)))
    modules))

(defun ocaml-module-alist ()
  "Call by need value of valriable ocaml-module-alist"
  (if (listp ocaml-module-alist)
      nil
    ;; build list of mli files
    (setq ocaml-module-alist (ocaml-add-mli-modules nil 'lib))
    ;; dumping information ? TODO
    )
  ocaml-module-alist)

(defun ocaml-get-or-make-module (module &optional tag)
  (let ((info (assoc module (ocaml-module-alist))))
    (if info nil
      (setq info (cons module (cons (cons 'local default-directory) 'lazy)))
      (setq ocaml-module-alist (cons info ocaml-module-alist))
      )
    info))

;; Symbols of module are lazily computed

(defun ocaml-module-filename (module)
  (let ((module (uncapitalise module)) (name))
    (or (file-exists-p (setq name (concat module ".mli")))
        ; (file-exists-p (setq name (concat module ".ml"))) 
        (file-exists-p
         (setq name (concat ocaml-lib-directory "/" module ".mli")))
        (setq name nil))
    name))

(defun ocaml-module-symbols (module-info)
  (let* ((module (car module-info))
         (tail (and module-info (cdr module-info)))
         (tag (caar tail))
         (dir (cdar tail))
         (file)
         (alist))
    (if (listp (cdr tail))
        (cdr tail)
      (if (equal tag 'info)
          (setq dir (car ocaml-lib-path)) ; XXX to be fixed
        )
      (setq file (concat dir (ocaml-uncapitalize module) ".mli"))
      (message file)
      (save-window-excursion
        (set-buffer (get-buffer-create "*ocaml-help*"))
        (if (and file (file-exists-p file))
            (progn
              (message "Scanning module %s" file)
              (insert-file-contents file))
          (message "Module %s not found" module))
        (while (re-search-forward
                "^\\([ \t]*val\\|let\\) \\([^ (:=]*\\)" (point-max) 'move)
          (setq alist (cons (match-string 2) alist)))
        (erase-buffer)
        )
      (setcdr tail alist)
      alist)
      ))

;; Local list of visible modules. 

(defvar ocaml-visible-modules 'lazy
  "A-list of open modules, local to every file.")
(make-variable-buffer-local 'ocaml-visible-modules)
(defun ocaml-visible-modules ()
  (if (listp ocaml-visible-modules) nil
    (progn
      (setq ocaml-visible-modules
            (list (ocaml-get-or-make-module "Pervasives")))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^ *open  *\\([A-Z][a-zA-Z'_0-9]*\\)"
                                  (point-max) t)
          (let ((module (match-string 1)))
            (if (member module ocaml-visible-modules) nil
              (setq ocaml-visible-modules
                    (cons (ocaml-get-or-make-module module)
                          ocaml-visible-modules)))))
        )))
  ocaml-visible-modules)

;; Look for identifiers aroun poin

(defun ocaml-current-position ()
  "Return a pair (MODULE . ENTRY) such that point is above ENTRY and 
MODULE is the module preceeding ENTRY.

Both are a pair of position (BEG . END) in the buffer and can be nil if
undefined."
  (let ((module) (entry))
    (save-excursion
      (backward-word 1)
      (if (looking-at "\\([A-Z][A-Za-z0-9_]*\\)[.]")
          (setq module (cons (match-beginning 1) (match-end 1)))
        (if (looking-at "[a-z_][A-Za-z0-9_]*")
            (progn
              (setq entry (cons (match-beginning 0) (match-end 0)))
              (backward-word 1)
              (if (looking-at
                   (concat "\\([A-Z][A-Za-z0-_]*\\)[.]"
                           (regexp-quote (match-string 0))))
                  (setq module (cons (match-beginning 1) (match-end 1)))))))
      (cons module entry))))

;; completion around point

(defun ocaml-completion (pattern module)
  (let ((list
         (or
          (and module
               (list 
                (or (assoc module (ocaml-module-alist))
                    (error "Unknown module %s" module))))
          (ocaml-visible-modules))))
    (message "Completion from %s" (mapconcat 'car list " "))
    (if (null pattern)
        (apply 'append (mapcar 'ocaml-module-symbols list))
      (let ((pat (concat "^" (regexp-quote pattern))) (res))
        (iter
         '(lambda (l)
            (iter '(lambda (x)
                     (if (string-match pat (car l))
                         (if (member x res) nil (setq res (cons x res)))))
                  (ocaml-module-symbols l)))
         list)
        res)
      )))

(defun ocaml-complete (arg)
  "Complete symbol define in libraries"
  (interactive "p")
  (let* ((module-entry (ocaml-current-position))
         (module)
         (entry (cdr module-entry))
         (beg) (end) (pattern))
    (if (car module-entry)
        (setq module
              (buffer-substring (caar module-entry) (cdar module-entry))))
    (if (consp (cdr module-entry))
        (progn         
          (setq beg (cadr module-entry))
          (setq end (cddr module-entry)))
      (if (and module
           (save-excursion
            (goto-char (cdar module-entry))
            (looking-at " *[.]")))
          (progn
            (setq beg (match-end 0))
            (setq end beg))))
    (if (not (and beg end))
        (error "Did not find anything to complete around point")
      (setq pattern (buffer-substring beg end))
      (let* ((table 'ocaml-completion)
             (all-completions (ocaml-completion pattern module))
             (completion
              (try-completion pattern (mapcar 'list all-completions))))
        (cond ((eq completion t))

              ((null completion)
               (let*
                   ((modules (ocaml-find-module pattern))
                    (module
                     (cond
                      ((null modules)
                       nil)
                      ((equal (length modules) 1)
                       (caar modules))
                      (t
                       (setq hist (mapcar 'car modules))
                       (completing-read "Module: " modules nil t
                                        "" (cons 'hist 0)))
                      )))
                 (if (null module)
                     (error "Can't find completion for \"%s\"" pattern)
                   (delete-region beg end)
                   (insert module "." pattern))))
                     
              ((not (string-equal pattern completion))
               (delete-region beg end)
               (insert completion))

              (t
               (with-output-to-temp-buffer "*Help*"
                 (display-completion-list all-completions))
               ))
               ))))


;; Info files


(defvar ocaml-module-regexp "[A-Z][A-Za-z0-9_]*")
; (defvar ocaml-info-section-regexp
;   (concat "\\* \\(Section [1-9][0-9--]*\\|" ocaml-module-regexp
;           "\\)::[ \t][ \t]*Module *\\(" ocaml-module-regexp "\\|\n\\)"))
(defvar ocaml-info-section-regexp
  (concat "\\* \\(Section [1-9][0-9--]*\\)::[ \t][ \t]*Module *\\(" ocaml-module-regexp "\\)"))
(defun ocaml-info-section ()
  (let ((section (match-string 1)) (module (match-string 2)))
    (if (string-equal module "\n") (setq module section))
    (cons module section)))

(defun ocaml-info-add-entries (entries dir name)
  (let*
      ((filter (concat "-type f -regex '.*/" name
                       "\\(.info\\|\\)\\(-[0-9]*\\|\\)\\([.]gz\\|\\)'"
                       ))
       (files (ocaml-find-files dir filter))
       (command))
    ;; scanning info files
    (if (or (null files)
            (not (stringp files))
            (string-match files "^ *$"))
        (message "No info file found: %s." (mapconcat 'identity files " "))
      (message "Scanning info files %s." files)
      (set-buffer (get-buffer-create "*ocaml-help*"))
      (setq command
            (concat "gunzip -c -f " files
                " | grep -e '" ocaml-info-section-regexp "'"))
      (message command)
      (or (shell-command command (current-buffer)) (error "HERE"))
      (goto-char (point-min))
      (while (re-search-forward ocaml-info-section-regexp (point-max) t)
        (let* ((module (match-string 2))
               (section (match-string 1)))
          (message "%s %s" module section)
          (if (assoc module entries) nil
            (setq entries
                  (cons (cons module (concat "(" name ")" section))
                        entries))
            )))
      (kill-buffer (current-buffer)))
    entries))

(defun ocaml-info-default-function ()
  "The default way to create an info data base from the value 
of \\[Info-default-directory-list] and the base name \\[ocaml-info-name] 
of files to look for."
  (let ((collect) (seen))
    (iter '(lambda (d)
             (if (member d seen) nil
               (setq collect
                     (ocaml-info-add-entries collect d ocaml-info-name-list))
               (setq done (cons d seen))))
          Info-directory-list)
    collect))

(defun ocaml-info-alist ()
  "Call by need value of variable ocaml-info-alist"
  (cond
   ((listp ocaml-info-alist))
   ((functionp ocaml-info-alist)
    (setq ocaml-info-alist (apply ocaml-info-alist nil)))
   (t
    (error "wrong type for ocaml-info-alist")))
  ocaml-info-alist)

;; help around point

(defun ocaml-find-module (symbol &optional module-list)
  (let ((list (or module-list (ocaml-module-alist)))
        (collect))
    (while (consp list)
      (if (member symbol (ocaml-module-symbols (car list)))
          (setq collect (cons (car list) collect)))
      (setq list (cdr list)))
    collect
    ))

(defun ocaml-buffer-substring (region)
  (and region (buffer-substring (car region) (cdr region))))

;; Help function. 

(defun ocaml-goto-help (&optional module entry)
  "Searches info manual for MODULE and ENTRY in MODULE.
If unspecified, MODULE and ENTRY are inferred from the position in the
current buffer using \\[ocaml-current-position]."
  (interactive)
  (let ((info-section (assoc module (ocaml-info-alist))))
    (if info-section (info (cdr info-section))
      (ocaml-visible-modules)
      (let* ((module-info (assoc module (ocaml-module-alist)))
             (location (cdadr module-info)))
        (cond
         (location
          (view-file (concat location (ocaml-uncapitalize module) ".mli"))
          (bury-buffer (current-buffer)))
         (info-section (error "Aborted"))
         (t (error "No help for module %s" (car module)))))
      ))
  (if (stringp entry)
      (let ((here (point)))
        (goto-char (point-min))
        (or (re-search-forward
             (concat "\\(val\\|exception\\|[|{;]\\) +" (regexp-quote entry))
             (point-max) t)
            (search-forward entry (point-max) t)
            (progn
              (message "Help for entry %s not found in module %s"
                       entry module)
              (goto-char here)))))
  )

(defun ocaml-help (arg)
  (interactive "p")
  (let ((module) (entry))
    (cond
     ((= arg 4)
      (or (setq module
                (completing-read "Module: " ocaml-module-alist nil t))
          (error "Quit")))
     (t
      (if (= arg 0) (setq ocaml-visible-modules 'lazy))
      (let ((module-entry (ocaml-current-position)))
        (setq entry (ocaml-buffer-substring (cdr module-entry)))
        (setq module
              (or (ocaml-buffer-substring (car module-entry))
                  (let ((modules
                         (or (ocaml-find-module entry (ocaml-visible-modules))
                             (ocaml-find-module entry)))
                         (hist))
                    (cond
                     ((null modules)
                      (error "No module found for entry %s" entry))
                     ((equal (length modules) 1)
                      (caar modules))
                     (t
                      (setq hist (mapcar 'car modules))
                      (completing-read "Module: " modules nil t
                                       "" (cons 'hist 0)))
                     ))))
        )))
     (message "Help for %s%s%s" module (if entry "." "") (or entry ""))
     (ocaml-goto-help module entry)
     ))


;; bindings

(if (boundp 'caml-mode-map)
    (progn 
      (define-key caml-mode-map [?\C-c?\C-h] 'ocaml-help)
      (define-key caml-mode-map [?\C-c?\t] 'ocaml-complete)
      ))

(provide 'caml-help)
