(defun cleverparens-lispy/init-evil-cleverparens ()
  (use-package evil-cleverparens
    :defer t
    :init
    (progn
      (add-hook 'emacs-lisp-mode-hook #'evil-cleverparens-mode)
      (setq evil-cleverparens-use-additional-bindings t))
    :config
    (require 'evil-cleverparens-text-objects)))

(defun cleverparens-lispy/init-lispy ()
  (use-package lispy
    :defer t
    :init
    (add-hook 'emacs-lisp-mode-hook (lambda ()
                                      (lispy-mode 1)
                                      (lispy-set-key-theme '(special evilcp c-digits))))))

(defun cleverparens-lispy/pre-init-smartparens ()
  (setq sp-highlight-pair-overlay nil)
  (setq sp-highlight-wrap-overlay nil)
  (setq sp-highlight-wrap-tag-overlay nil))

(defun cleverparens-lispy/post-init-smartparens ()
  (show-smartparens-global-mode -1))

(defun cleverparens-lispy/post-init-evil ()
  (setq evil-move-beyond-eol t))
