(setq magit-push-always-verify nil)
(setq magit-popup-show-common-commands nil)
(setq magit-auto-revert-mode t)
(setq magit-revert-buffers 1)
(setq magit-commit-show-diff nil)

;; Start commit in insert mode
(add-hook 'git-commit-mode-hook 'evil-insert-state)

;; Use C-n/C-p to navigate sections
(with-eval-after-load 'magit
  (evil-define-key 'normal magit-mode-map (kbd "C-n") 'magit-section-forward-sibling)
  (evil-define-key 'normal magit-mode-map (kbd "C-p") 'magit-section-backward-sibling)

  ;; Shackle magit status to right hand side
  (with-eval-after-load 'shackle
    (push '(magit-status-mode :align right :size 90) shackle-rules)))

(provide 'init-magit)
