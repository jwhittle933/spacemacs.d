(setq js2-mode-show-strict-warnings nil)
(setq js2-mode-show-parse-errors nil)
(setq js-indent-level 2)
(setq js2-basic-offset 2)
(setq js2-strict-trailing-comma-warning nil)
(setq js2-strict-missing-semi-warning nil)

;; Set jsx indentation offset to same as js js
(add-hook 'js2-jsx-mode-hook #'set-jsx-indentation)
(defun set-jsx-indentation ()
  (setq-local sgml-basic-offset js2-basic-offset))

(add-hook 'rjsx-mode-hook #'eslintd-set-flycheck-executable)
(add-hook 'rjsx-mode-hook #'flycheck-mode)
(add-hook 'rjsx-mode-hook #'add-node-modules-path)

(spacemacs|use-package-add-hook company-flow
  :post-init
  (progn
    (setq company-backends-js2-mode '((company-flow :with company-dabbrev)
                                      company-files
                                      company-dabbrev))
    (setq company-backends-react-mode '((company-flow :with company-dabbrev)
                                        company-files
                                        company-dabbrev))))

(with-eval-after-load 'js2-mode
  (modify-syntax-entry ?_ "w" js2-mode-syntax-table))

(defun eslintd-set-flycheck-executable ()
  (interactive)
  (when-let (eslintd-executable (executable-find "eslint_d"))
    (make-variable-buffer-local 'flycheck-javascript-eslint-executable)
    (setq flycheck-javascript-eslint-executable eslintd-executable)))

;; Set up flycheck for javascript
(with-eval-after-load 'flycheck
  (push 'javascript-jshint flycheck-disabled-checkers)
  (push 'json-jsonlint flycheck-disabled-checkers)
  (push 'js2-jsx-mode flycheck-global-modes)

  (flycheck-add-mode 'javascript-eslint 'web-mode))

;; Stop web-mode from using block comments in comment-dwim.
(with-eval-after-load 'web-mode
  (add-to-list 'web-mode-comment-formats '("jsx" . "//"))
  (setq web-mode-comment-formats
        (-map-when (lambda (i) (equal (car i) "javascript"))
                   (lambda (i) '("javascript" . "//"))
                   web-mode-comment-formats)))

;; Runs eslint --fix after save
(defun eslint-fix ()
  (interactive)
  (let ((current-point (point))
        (line (count-screen-lines (window-start) (point)))
        (command (concat
                  flycheck-javascript-eslint-executable
                  " --stdin"
                  " --fix-to-stdout"
                  " --stdin-filename " buffer-file-name))
        (buffer (current-buffer))
        (text (buffer-substring-no-properties (point-min) (point-max))))
    (with-temp-buffer
      (insert text)
      (when (eq 0
                (shell-command-on-region
                 ;; Region
                 (point-min)
                 (point-max)
                 ;; Command
                 command
                 ;; Output to current buffer
                 t
                 ;; Replace buffer
                 t
                 ;; Error buffer name
                 "*eslint-fix error*"))
        (let ((fixed-text (buffer-substring-no-properties (point-min) (point-max))))
          (with-current-buffer buffer
            (unless (string= fixed-text
                             (buffer-substring-no-properties (point-min) (point-max)))
              (delete-region (point-min) (point-max))
              (insert fixed-text)
              ;; Restore point and scroll position
              (goto-char current-point)
              (recenter (- line 1)))))))))

(add-hook 'js-mode-hook
          (lambda () (add-hook 'before-save-hook #'eslint-fix nil t)))
(add-hook 'react-mode-hook
          (lambda () (add-hook 'before-save-hook #'eslint-fix nil t)))

;; Monkey patch to fix indentation for attributes in jsx
(load-file "~/.spacemacs.d/lisp/sgml-mode-patch.el")
(require 'sgml-mode)

(provide 'init-javascript)
