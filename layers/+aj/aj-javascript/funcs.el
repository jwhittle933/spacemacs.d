(defun aj-javascript//locate-npm-executable (name)
  (let* ((node-module-path (concat "node_modules/.bin/" name))
         (dir (locate-dominating-file buffer-file-name node-module-path)))
    (if dir
        (concat dir node-module-path)
      (executable-find name))))

(defun aj-javascript/set-eslint-executable ()
  (interactive)
  (when-let* ((executable (aj-javascript//locate-npm-executable "eslint_d")))
    (setq-local eslintd-fix-executable executable)
    (setq-local flycheck-javascript-eslint-executable executable)))

(defun aj-javascript/set-prettier-command ()
  (interactive)
  (when-let* ((executable (aj-javascript//locate-npm-executable "prettier")))
    (setq-local prettier-js-command executable)))

;; Inspired by http://blog.binchen.org/posts/indent-jsx-in-emacs.html
(defun aj-javascript/js-jsx-indent-line-align-closing-bracket ()
  "Workaround sgml-mode and align closing bracket with opening bracket"
  (save-excursion
    (beginning-of-line)
    (when (looking-at-p "^ +\/?> *$")
      (delete-char sgml-basic-offset))))
