(with-eval-after-load 'bracketed-paste
  (defadvice bracketed-paste--tty-state-exit-hook (around fix)
    "This hook fails if the selected frame is graphical, so prevent it"
    (unless (display-graphic-p)
      ad-do-it))
  (ad-activate 'bracketed-paste--tty-state-exit-hook))

(provide 'fix-bracketed-paste)
