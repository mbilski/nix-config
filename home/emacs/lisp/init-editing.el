;; editing

;; flycheck
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(add-hook 'after-init-hook #'global-flycheck-mode)

;; magit
(use-package magit)
(global-set-key (kbd "C-x g") 'magit-status)

;; company
(use-package company :ensure t)
(add-hook 'after-init-hook 'global-company-mode)

;; ranger
(use-package ranger)
(global-set-key (kbd "C-c n") 'ranger)

;; evil
(use-package evil
  :init (evil-mode 1))

(use-package evil-commentary
  :ensure t
  :config
  (evil-commentary-mode))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-escape
  :diminish evil-escape-mode
  :config (evil-escape-mode))

(define-key evil-normal-state-map (kbd "C-u") (lambda ()
            (interactive)
            (evil-scroll-up nil)))

(define-key evil-normal-state-map (kbd "C-d") (lambda ()
            (interactive)
            (evil-scroll-down nil)))

;; evil-multiedit
(use-package evil-multiedit)

;; Highlights all matches of the selection in the buffer.
(define-key evil-visual-state-map "R" 'evil-multiedit-match-all)

;; Match the word under cursor (i.e. make it an edit region). Consecutive presses will
;; incrementally add the next unmatched match.
(define-key evil-normal-state-map (kbd "M-d") 'evil-multiedit-match-and-next)
;; Match selected region.
(define-key evil-visual-state-map (kbd "M-d") 'evil-multiedit-and-next)
;; Insert marker at point
(define-key evil-insert-state-map (kbd "M-d") 'evil-multiedit-toggle-marker-here)

;; Same as M-d but in reverse.
(define-key evil-normal-state-map (kbd "M-D") 'evil-multiedit-match-and-prev)
(define-key evil-visual-state-map (kbd "M-D") 'evil-multiedit-and-prev)

;; OPTIONAL: If you prefer to grab symbols rather than words, use
;; `evil-multiedit-match-symbol-and-next` (or prev).

;; Restore the last group of multiedit regions.
(define-key evil-visual-state-map (kbd "C-M-D") 'evil-multiedit-restore)

;; RET will toggle the region under the cursor
(define-key evil-multiedit-state-map (kbd "RET") 'evil-multiedit-toggle-or-restrict-region)

;; ...and in visual mode, RET will disable all fields outside the selected region
(define-key evil-motion-state-map (kbd "RET") 'evil-multiedit-toggle-or-restrict-region)

;; For moving between edit regions
(define-key evil-multiedit-state-map (kbd "C-n") 'evil-multiedit-next)
(define-key evil-multiedit-state-map (kbd "C-p") 'evil-multiedit-prev)
(define-key evil-multiedit-insert-state-map (kbd "C-n") 'evil-multiedit-next)
(define-key evil-multiedit-insert-state-map (kbd "C-p") 'evil-multiedit-prev)

;; Ex command that allows you to invoke evil-multiedit with a regular expression, e.g.
(evil-ex-define-cmd "ie[dit]" 'evil-multiedit-ex-match)

;; counsel
(use-package counsel)
(global-set-key (kbd "M-p") 'counsel-yank-pop)

;; smartparent
(use-package smartparens :ensure t)
(global-set-key (kbd "M-l") 'sp-forward-sexp)

;; helm
(use-package helm)
(use-package helm-ag)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-m") 'helm-M-x)
(global-set-key (kbd "C-c C-m") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

;; editorconfig
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;; projectile
(use-package projectile
  :demand
  :init (setq
         projectile-use-git-grep t
         helm-ag-insert-at-point 'word)
  :config (projectile-global-mode t)
  :bind   (("M-f" . projectile-find-file)
           ("M-F" . projectile-find-file-in-known-projects)
           ("M-r" . helm-projectile-ag)))

(global-set-key (kbd "C-c p p") 'helm-projectile-switch-project)

(use-package helm-projectile
  :init
  (helm-projectile-on))

(setq projectile-switch-project-action 'helm-projectile-find-file)

;; zoom-window
(use-package zoom-window
  :bind ("C-x C-z" . zoom-window-zoom))

;; move-text
(use-package expand-region)
(use-package move-text)

(require 'expand-region)
(global-set-key (kbd "M-e") 'er/expand-region)
(global-set-key (kbd "M-E") 'er/contract-region)

;; which-key
(use-package which-key
  :init (which-key-mode))

(setq which-key-idle-delay 0.5)

;;
(use-package window-numbering
  :init (window-numbering-mode))

;; linum-relative
(use-package linum-relative
  :init (linum-relative-global-mode))

;; server
(load "server")
(unless (server-running-p) (server-start))

;; global keybindings
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "<home>") 'back-to-indentation)
(global-set-key (kbd "<end>") 'move-end-of-line)
(global-set-key (kbd "<S-up>") 'move-text-up)
(global-set-key (kbd "<S-down>") 'move-text-down)

(global-set-key (kbd "M-]") 'next-buffer)
(global-set-key (kbd "M-[") 'previous-buffer)

(global-set-key [next]
  (lambda () (interactive)
    (condition-case nil (scroll-up)
      (end-of-buffer (goto-char (point-max))))))

(global-set-key [prior]
  (lambda () (interactive)
    (condition-case nil (scroll-down)
      (beginning-of-buffer (goto-char (point-min))))))

;; indentation
(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(global-set-key (kbd "C-c <tab>") 'indent-buffer)

;; hooks
(add-hook 'prog-mode-hook
  (lambda ()
    (rainbow-delimiters-mode)))

(add-hook 'prog-mode-hook
  (lambda ()
    (smartparens-mode)))

(provide 'init-editing)
