(defconst aj-javascript-packages
  '(
    add-node-modules-path
    company-flow
    eslintd-fix
    flow-minor-mode
    flycheck
    prettier-js
    (flow-js2-mode :location (recipe :fetcher github :repo "Fuco1/flow-js2-mode"))
    rjsx-mode))


(defun aj-javascript/init-eslintd-fix ()
  (use-package eslintd-fix
    :defer t
    :commands eslintd-fix-mode
    :init
    (progn
      (add-hook 'rjsx-mode-hook #'eslintd-fix-mode t))))

(defun aj-javascript/init-rjsx-mode ()
  (use-package rjsx-mode
    :defer t
    :init
    (progn
      (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))

      (setq
       js2-mode-show-strict-warnings nil
       js2-mode-show-parse-errors nil
       js-indent-level 2
       js2-basic-offset 2
       js2-strict-trailing-comma-warning nil
       js2-strict-missing-semi-warning nil)

      (advice-add #'js-jsx-indent-line
                  :after
                  #'aj-javascript/js-jsx-indent-line-align-closing-bracket)
      (add-hook 'rjsx-mode-hook #'aj-javascript/set-eslint-executable t)
      (add-hook 'rjsx-mode-hook #'aj-javascript/set-flow-executable t))
    :config
    (modify-syntax-entry ?_ "w" js2-mode-syntax-table)))

(defun aj-javascript/post-init-add-node-modules-path ()
  (with-eval-after-load 'rjsx-mode
    (add-hook 'rjsx-mode-hook #'add-node-modules-path)))

(defun aj-javascript/post-init-company-flow ()
  (spacemacs|add-company-backends
    :backends
    '((company-flow :with company-dabbrev-code)
      company-files)))

(defun aj-javascript//flycheck-eslint-disable-prettier (oldfun checker &rest args)
  (let ((arguments (apply oldfun checker args)))
    (if (eq checker 'javascript-eslint)
        (cons "--rule=prettier/prettier:off" arguments)
      arguments)))

(defun aj-javascript/post-init-flycheck ()
  (with-eval-after-load 'flycheck
    (advice-add 'flycheck-checker-substituted-arguments :around
                'aj-javascript//flycheck-eslint-disable-prettier)

    (push 'javascript-jshint flycheck-disabled-checkers)
    (push 'json-jsonlint flycheck-disabled-checkers))

  (spacemacs/enable-flycheck 'rjsx-mode))

(defun aj-javascript//enable-prettier ()
  "Enable prettier if there is a config and we're not in node_modules"
  (when (and (locate-dominating-file buffer-file-name "prettier.config.js")
             (not (string-match "\/node_modules\/" buffer-file-name)))
    (aj-javascript/set-prettier-command)
    (prettier-js-mode)))

(defun aj-javascript//enable-prettier-in-web-mode ()
  "Enable prettier in web mode if we're in a tsx file"
  (when (and buffer-file-name
           (string-match "\\.tsx?\\'" buffer-file-name))
    (aj-javascript//enable-prettier)))

(defun aj-javascript/init-prettier-js ()
  (use-package prettier-js
    :defer t
    :init
    (progn)))
      ;; (add-hook 'rjsx-mode-hook 'aj-javascript//enable-prettier)
      ;; (add-hook 'web-mode-hook 'aj-javascript//enable-prettier-in-web-mode)


(defun aj-javascript/init-flow-minor-mode ()
  (use-package flow-minor-mode
    :defer t
    :init
    (add-hook 'rjsx-mode-hook 'flow-minor-enable-automatically)))

(defun aj-javascript/init-flow-js2-mode ()
  (use-package flow-js2-mode
    :defer t
    :init
    (add-hook 'rjsx-mode-hook 'flow-js2-mode)))

