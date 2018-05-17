;; go

(use-package go-mode :ensure t)
(use-package go-guru :ensure t)
(use-package gotest :ensure t)

(defun custom-go-mode-hook ()
  (setq gofmt-command "goimports")
  (go-guru-hl-identifier-mode)
  (add-hook 'before-save-hook 'gofmt-before-save)
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v"))
  (local-set-key (kbd "C-c C-c") 'go-test-current-project)
  (local-set-key (kbd "M-p") 'compile)
  (local-set-key (kbd "M-n") 'next-error)
  (local-set-key (kbd "M-P") 'recompile)
  (local-set-key (kbd "M-*") 'pop-tag-mark))
(add-hook 'go-mode-hook 'custom-go-mode-hook)

(evil-define-key 'normal go-mode-map (kbd "M-.") 'godef-jump)

;; exec-path-from-shell
(use-package exec-path-from-shell
  :init (exec-path-from-shell-initialize)
        (exec-path-from-shell-copy-env "GOPATH"))

(add-hook 'go-mode-hook (lambda ()
                          (company-mode)
                          (set (make-local-variable 'company-backends) '((company-go company-ctags company-capf company-abbrev company-dabbrev-code)))))

(provide 'init-go)
