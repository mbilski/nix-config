;; elm

;; flycheck-elm
(use-package flycheck-elm)

(eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-elm-setup))

;; elm
(use-package elm-mode)

(setq elm-tags-exclude-elm-stuff nil)
(setq elm-format-on-save t)

(defun find-tag-no-prompt ()
  (interactive)
  (find-tag (find-tag-default)))

(evil-define-key 'normal elm-mode-map (kbd "M-.") 'find-tag-no-prompt)

(add-hook 'elm-mode-hook
          (lambda ()
          (setq company-backends '((company-elm company-ctags company-capf company-abbrev company-dabbrev-code)))))

(provide 'init-elm)
