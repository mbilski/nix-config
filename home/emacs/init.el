;; (package-initialize)

(let ((lisp-directory (expand-file-name "lisp" user-emacs-directory)))
  ;; add ~/.emacs.d/lisp to load path
  (add-to-list 'load-path lisp-directory)

  ;; save customizations as local (unversioned) settings
  (setq custom-file (expand-file-name "init-local.el" lisp-directory)))

;; load files
(require 'init-package)
(require 'init-appearance)
(require 'init-editing)
(require 'init-elm)
(require 'init-go)
(require 'init-others)
