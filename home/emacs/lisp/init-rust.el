;; rust

(use-package rust-mode :ensure t)
(use-package cargo :ensure t)

(add-hook 'rust-mode-hook 'cargo-minor-mode)

(add-hook 'rust-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c <tab>") #'rust-format-buffer)
            (local-set-key (kbd "C-c .") #'racer-find-definition)))

(use-package racer :ensure t)

(setq racer-cmd "racer")
(setq racer-rust-src-path "/home/mbilski/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src")

(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)

(use-package flycheck-rust :ensure t)
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

(provide 'init-rust)
