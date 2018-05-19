;; init others

(use-package nix-mode)

;; json
(use-package json-mode)

;; yaml
(use-package yaml-mode)

;; markdown
(use-package markdown-mode)

;; cabal
(let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
  (setenv "PATH" (concat my-cabal-path path-separator (getenv "PATH")))
  (add-to-list 'exec-path my-cabal-path))

;; haskell
(use-package intero)
(add-hook 'haskell-mode-hook 'intero-mode)

;; latex
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-save-query nil)

(provide 'init-others)
