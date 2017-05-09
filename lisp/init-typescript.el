(setq typescript-indent-level 2)

;; Work around https://github.com/ananthakumaran/tide/issues/144
;; Remove after https://github.com/ananthakumaran/tide/pull/146 is merged
(defun aj/fix-tide-start-server (start-server &rest args)
  (let ((process-connection-type nil))
    (apply start-server args)))
(with-eval-after-load 'tide
  (advice-add 'tide-start-server :around 'aj/fix-tide-start-server)
  (add-hook 'before-save-hook #'tide-format-before-save))

(provide 'init-typescript)
