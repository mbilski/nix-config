;; globals
(setq
 inhibit-startup-screen t
 inhibit-startup-message t
 create-lockfiles nil
 make-backup-files nil
 column-number-mode t
 scroll-error-top-bottom t
 make-backup-files nil
 auto-save-default nil
 sentence-end-double-space nil)

(setq-default
 indent-tabs-mode nil
 fill-column 100
 tab-width 2
 js-indent-level 2
 c-basic-offset 2)

(fset `yes-or-no-p `y-or-n-p)

(set-face-attribute 'default nil
                    :family "Hack"
                    :height 100
                    :weight 'normal
                    :width 'normal)

(setq mac-command-modifier 'super)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)

;; disable dialogs
(setq use-dialog-box nil)

;; highlight matching parentheses
(use-package paren
  :ensure nil
  :config
  (show-paren-mode 1))

;; themes
(use-package dracula-theme
  :ensure t
  :config)

(use-package moe-theme)

(if (display-graphic-p)
    (load-theme 'dracula t)
    (load-theme 'moe-dark t))

;; rainbow-delimiters
(use-package rainbow-delimiters
  :init (rainbow-delimiters-mode))

(provide 'init-appearance)
