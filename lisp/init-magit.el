(setq magit-push-always-verify nil)
(setq magit-popup-show-common-commands nil)
(setq magit-auto-revert-mode t)
(setq magit-revert-buffers 1)
(setq magit-commit-show-diff nil)
(setq magit-display-buffer-function 'magit-display-buffer-fullcolumn-most-v1)

;; Start commit in insert mode
(add-hook 'git-commit-mode-hook 'evil-insert-state)

;; Use C-n/C-p to navigate sections
(with-eval-after-load 'magit
  (evil-define-key 'normal magit-mode-map (kbd "C-n") 'magit-section-forward-sibling)
  (evil-define-key 'normal magit-mode-map (kbd "C-p") 'magit-section-backward-sibling))

(provide 'init-magit)
