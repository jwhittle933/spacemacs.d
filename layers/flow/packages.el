(defconst flow-packages
  '((company-flow :toggle (configuration-layer/package-usedp 'company))
    (company-tern :excluded t)
    (flow-js2-mode :location (recipe :fetcher github :repo "Fuco1/flow-js2-mode"))
    flow-minor-mode
    (flycheck-flow :toggle (configuration-layer/package-usedp 'flycheck))
    rjsx-mode
    (tern :excluded t)))

(defun flow/init-company-flow ()
  (use-package company-flow
    :defer t
    :commands company-flow
    :init
    (spacemacs|add-company-backends
      :backends
      (company-flow :with company-dabbrev-code)
      company-files
      :modes js2-mode rjsx-mode)
    :config
    (add-to-list 'company-flow-modes 'rjsx-mode)))

(defun flow/init-flow-js2-mode ()
  (use-package flow-js2-mode
    :defer t
    :init
    (add-hook 'rjsx-mode-hook 'flow-js2-mode)))

(defun flow/init-flow-minor-mode ()
  (use-package flow-minor-mode
    :defer t
    :init
    (add-hook 'rjsx-mode-hook 'flow-minor-enable-automatically)))

(defun flow/init-flycheck-flow ()
  (use-package flycheck-flow
    :config
    (progn
      (flycheck-add-mode 'javascript-flow 'react-mode)
      (flycheck-add-mode 'javascript-flow 'rjsx-mode)
      (flycheck-add-next-checker 'javascript-flow 'javascript-eslint))))

(defun flow/post-init-rjsx-mode ()
  (add-hook 'rjsx-mode-hook #'flow/set-flow-executable t))
