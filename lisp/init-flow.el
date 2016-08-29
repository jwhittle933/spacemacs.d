(use-package flycheck-flow
  :config
  (progn
    ;; Run flow first
    (setq flycheck-checkers (remove 'javascript-flow flycheck-checkers))
    (add-to-list 'flycheck-checkers 'javascript-flow)
    (flycheck-add-mode 'javascript-flow 'react-mode)
    (flycheck-add-next-checker 'javascript-flow '(t . javascript-eslint))))

(require 'company-flow)
(add-to-list 'company-flow-modes 'react-mode)

(require 'flow-types)

(provide 'init-flow)
