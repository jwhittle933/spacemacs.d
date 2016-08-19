(use-package flycheck-flow
  :config
  (progn
    (flycheck-add-mode 'javascript-flow 'web-mode)
    (flycheck-add-mode 'javascript-flow 'react-mode)
    (flycheck-add-next-checker 'javascript-eslint '(t . javascript-flow))))

(require 'flow-types)

(provide 'init-flow)
