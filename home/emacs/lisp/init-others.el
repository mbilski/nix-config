;; init others

;; json
(use-package json-mode)

;; yaml
(use-package yaml-mode)

;; markdown
(use-package markdown-mode)

;; git-timemachine
(use-package git-timemachine)
(global-set-key (kbd "M-t") 'git-timemachine)

(eval-after-load 'git-timemachine
  '(progn
     (evil-make-overriding-map git-timemachine-mode-map 'normal)
     ;; force update evil keymaps after git-timemachine-mode loaded
     (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps)))

(provide 'init-others)
