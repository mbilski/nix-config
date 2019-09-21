;; go

(use-package lsp-mode)

(use-package lsp-ui
  :commands lsp-ui-mode)

(setq lsp-ui-sideline-enable nil)
(evil-define-key 'normal go-mode-map (kbd "M-l") 'lsp-restart-workspace)

(use-package company-lsp
  :commands company-lsp)

(use-package go-mode :ensure t)
(use-package go-guru :ensure t)
(use-package gotest :ensure t)
(use-package protobuf-mode :ensure t)
(use-package company-go :ensure t)

(defun custom-go-mode-hook ()
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (local-set-key (kbd "C-c C-c") 'go-test-current-project))

(add-hook 'go-mode-hook 'custom-go-mode-hook)

(evil-define-key 'normal go-mode-map (kbd "M-.") 'lsp-find-definition)
(evil-define-key 'normal go-mode-map (kbd "M-q") 'next-error)

(use-package yasnippet)
(yas-global-mode 1)
(add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")

;; exec-path-from-shell
(use-package exec-path-from-shell
  :init (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH"))

(add-hook 'go-mode-hook (lambda () (setq flycheck-disabled-checkers '(go-golint go-vet go-test go-fmt go-staticcheck go-build go-megacheck go-unconvert go-errcheck))))

(add-hook 'go-mode-hook #'lsp-deferred)

(setq lsp-prefer-flymake nil)
(setq lsp-ui-flycheck-live-reporting nil)
(setq company-idle-delay .3)
(setq company-echo-delay 0)
(electric-pair-mode 1)

(provide 'init-go)
