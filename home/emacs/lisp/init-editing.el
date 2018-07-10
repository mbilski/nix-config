;; editing

;; flycheck
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(add-hook 'after-init-hook #'global-flycheck-mode)

;; magit
(use-package magit)
(global-set-key (kbd "C-x g") 'magit-status)

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

;; multiple cursors
(use-package evil-mc)
(global-evil-mc-mode  1)

;; helm
(use-package helm)

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
  :init (setq projectile-use-git-grep t)
  :config (projectile-global-mode t)
  :bind   (("M-f" . projectile-find-file)
           ("M-F" . projectile-find-file-in-known-projects)
           ("M-r" . helm-projectile-grep)))

(use-package helm-projectile
  :init
  (helm-projectile-on))

(setq projectile-switch-project-action 'helm-projectile-find-file)

;; undo
(use-package undo-tree
  :diminish undo-tree-mode
  :config (global-undo-tree-mode)
  :bind ("M-/" . undo-tree-visualize))

;; zoom-window
(use-package zoom-window
  :bind ("C-x C-z" . zoom-window-zoom))

;; move-text
(use-package expand-region)
(use-package move-text)

(require 'expand-region)
(global-set-key (kbd "M-e") 'er/expand-region)
(global-set-key (kbd "M-E") 'er/contract-region)

;; neotree
 (use-package neotree
   :init (setq neo-smart-open t)
         (setq neo-autorefresh nil)
         (setq neo-theme 'arrow))

(defun neotree-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (projectile-project-root))
        (file-name (buffer-file-name)))
    (neotree-toggle)
    (if project-dir
        (if (neo-global--window-exists-p)
            (progn
              (neotree-dir project-dir)
              (neotree-find file-name)))
              (message "Could not find git project root."))))

(global-set-key (kbd "C-c n") 'neotree-project-dir)

(evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
(evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)

;; which-key
(use-package which-key
  :init (which-key-mode))

(setq which-key-idle-delay 0.5)

;; windmove
(use-package windmove
  :init (windmove-default-keybindings))

;;
(use-package window-numbering
  :init (window-numbering-mode))

;; git-gutter
(use-package git-gutter
  :init (global-git-gutter-mode +1))

;; linum-relative
(use-package linum-relative
  :init (linum-relative-global-mode))

;; server
(load "server")
(unless (server-running-p) (server-start))

;; smartparentheses
(use-package smartparens
  :diminish smartparens-mode
  :commands
  smartparens-strict-mode
  smartparens-mode
  sp-restrict-to-pairs-interactive
  sp-local-pair
  :init
  (setq sp-interactive-dwim t)
  :config
  (require 'smartparens-config)
  (sp-use-smartparens-bindings)

  (sp-pair "(" ")" :wrap "C-(")
  (sp-pair "[" "]" :wrap "s-[")
  (sp-pair "\"" "\"" :wrap "C-\"")
  (sp-pair "{" "}" :wrap "C-{"))

;; completions
 (use-package company
   :diminish company-mode
   :commands company-mode
   :init
   (setq
    company-idle-delay 0
    company-minimum-prefix-length 2)
   :config)

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

;; hooks
(add-hook 'prog-mode-hook
  (lambda ()
    (global-company-mode)
    (rainbow-delimiters-mode)
    (smartparens-global-mode)))

(provide 'init-editing)
