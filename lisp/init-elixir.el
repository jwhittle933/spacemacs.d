;; Elixir
;; Treat _ as a word character
(with-eval-after-load 'elixir-mode
  (modify-syntax-entry ?_ "w" elixir-mode-syntax-table))

(setq alchemist-test-ask-about-save nil)

;; Add flychecker for dialyzer
(with-eval-after-load 'flycheck
  (flycheck-define-checker elixir-dialyzer
    "Erlang syntax checker based on dialyzer."
    :command ("mix" "dialyzer")
    :predicate
    (lambda ()
      (and
       (buffer-file-name)
       (file-exists-p "mix.exs")
       (file-exists-p "deps/dialyxir")
       (file-exists-p ".local.plt")))
    :error-patterns
    ((error line-start
            (file-name)
            ":"
            line
            ":"
            (message)
            line-end))
    :modes elixir-mode)

  (add-to-list 'flycheck-checkers 'elixir-dialyzer t))

;; Change default-directory to where the mix.exs is so dialyzer works
;; Better solution here: https://github.com/flycheck/flycheck/pull/813
(add-hook 'elixir-mode-hook (lambda ()
                              (let ((mix-path (locate-dominating-file default-directory "mix.exs")))
                                (when mix-path
                                  (setq default-directory mix-path)))
                              (flycheck-mode)))

;; Pin alchemist windows to bottom
(push '("*alchemist test report*"
        :position bottom :noselect t :dedicated t :stick t :height 25)
      popwin:special-display-config)
(push '("*alchemist mix*"
        :position bottom :noselect t :dedicated t :stick t :height 25)
      popwin:special-display-config)
(push '("*alchemist help*"
        :position bottom :noselect t :dedicated t :stick t :height 25)
      popwin:special-display-config)
(push '("*Alchemist-IEx*"
        :position bottom :noselect t :dedicated t :stick t :height 25)
      popwin:special-display-config)

(provide 'init-elixir)
