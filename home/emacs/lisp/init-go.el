;; go

(use-package go-mode :ensure t)
(use-package go-guru :ensure t)
(use-package gotest :ensure t)

(use-package protobuf-mode :ensure t)

(defun custom-go-mode-hook ()
  (setq gofmt-command "goimports")
  (go-guru-hl-identifier-mode)
  (add-hook 'before-save-hook 'gofmt-before-save)
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v"))
  (local-set-key (kbd "C-c C-c") 'go-test-current-project))
(add-hook 'go-mode-hook 'custom-go-mode-hook)

(evil-define-key 'normal go-mode-map (kbd "M-.") 'godef-jump)

;; exec-path-from-shell
(use-package exec-path-from-shell
  :init (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH"))

(add-hook 'go-mode-hook (lambda ()
                          (set (make-local-variable 'company-backends) '(company-go))
                          (setq flycheck-disabled-checkers '(go-megacheck go-unconvert go-errcheck))
                          (company-mode)))

(provide 'init-go)
