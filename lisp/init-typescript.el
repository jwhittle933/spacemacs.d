(setq typescript-indent-level 2)

(with-eval-after-load 'tide
  (spacemacs/set-leader-keys-for-minor-mode 'tide-mode "f" #'tide-fix))

(provide 'init-typescript)
