(setq typescript-indent-level 2)

(with-eval-after-load 'tide
  (add-hook 'before-save-hook #'tide-format-before-save))

(provide 'init-typescript)
