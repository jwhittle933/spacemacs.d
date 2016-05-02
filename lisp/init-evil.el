;; Enable evil-mc for only prog and text, so it doesn't screw w/ magit and such
(with-eval-after-load 'evil-mc
  (add-hook 'prog-mode-hook 'turn-on-evil-mc-mode)
  (add-hook 'text-mode-hook 'turn-on-evil-mc-mode))

(with-eval-after-load 'evil
  ;; Move text up and down in visual mode with J/K
  (define-key evil-visual-state-map "J"
    (concat ":move '>+1" (kbd "RET") "gv=gv"))
  (define-key evil-visual-state-map "K"
    (concat ":move '<-2" (kbd "RET") "gv=gv"))

  ;; make j & k behave as g j & g k:
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

  ;; Use more simple */# so we don't drop into symbol-highlight mode
  (define-key evil-normal-state-map (kbd "*") 'evil-search-word-forward)
  (define-key evil-normal-state-map (kbd "#") 'evil-search-word-backward)

  ;; Remove this mapping so C-z can suspend in tty
  (define-key evil-motion-state-map (kbd "C-z") nil))

;; search by symbols in emacs
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq evil-symbol-word-search t)))

;; Make > and < round to tab stops
(setq evil-shift-round nil)

;; Account our collective ability to hit escape and then another key quickly
(setq evil-esc-delay 0.001)

(provide 'init-evil)
