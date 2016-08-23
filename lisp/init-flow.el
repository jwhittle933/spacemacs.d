(use-package flycheck-flow
  :config
  (progn
    ;; Run flow first
    (setq flycheck-checkers (remove 'javascript-flow flycheck-checkers))
    (add-to-list 'flycheck-checkers 'javascript-flow)
    (flycheck-add-mode 'javascript-flow 'web-mode)
    (flycheck-add-mode 'javascript-flow 'react-mode)
    (flycheck-add-next-checker 'javascript-flow '(t . javascript-eslint))))

(require 'flow-types)

(provide 'init-flow)
