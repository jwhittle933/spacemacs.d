;; Uses https://github.com/luxbock/evil-cleverparens and
;; https://github.com/abo-abo/lispy via
;; https://github.com/abo-abo/lispy/pull/174

(add-hook 'emacs-lisp-mode-hook #'evil-cleverparens-mode)
(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (lispy-mode 1)
                                  (lispy-set-key-theme '(special evilcp c-digits))))
(setq evil-move-beyond-eol t)
(setq evil-cleverparens-use-additional-bindings t)
(require 'evil-cleverparens-text-objects)

;; Smartparens
(setq sp-highlight-pair-overlay nil)
(setq sp-highlight-wrap-overlay nil)
(setq sp-highlight-wrap-tag-overlay nil)

;; Disable smartparens highlighting
(with-eval-after-load 'smartparens
  (show-smartparens-global-mode -1))

(provide 'init-lisp)
