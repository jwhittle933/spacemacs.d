(defun aj-javascript/eslintd-set-flycheck-executable ()
  (interactive)
  (when-let (eslintd-executable (executable-find "eslint_d"))
    (make-variable-buffer-local 'flycheck-javascript-eslint-executable)
    (setq flycheck-javascript-eslint-executable eslintd-executable)))

;; Inspired by http://blog.binchen.org/posts/indent-jsx-in-emacs.html
(defun aj-javascript/js-jsx-indent-line-align-closing-bracket ()
  "Workaround sgml-mode and align closing bracket with opening bracket"
  (save-excursion
    (beginning-of-line)
    (when (looking-at-p "^ +\/?> *$")
      (setf (buffer-substring (line-beginning-position)
                              (+ sgml-basic-offset (line-beginning-position)))
            ""))))
