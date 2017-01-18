(defconst aj-emacs-lisp-packages
  '(
    eros
    nameless
    ))

(defun aj-emacs-lisp/init-eros ()
  (use-package eros
    :defer t
    :init
    (add-hook 'emacs-lisp-mode-hook #'eros-mode)))

(defun aj-emacs-lisp/init-nameless ()
  (use-package nameless
    :defer t
    :init
    (add-hook 'emacs-lisp-mode-hook #'nameless-mode)))
