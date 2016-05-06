;; Elixir
;; Treat _ as a word character
(with-eval-after-load 'elixir-mode
  (modify-syntax-entry ?_ "w" elixir-mode-syntax-table))

(setq alchemist-test-ask-about-save nil)
(setq alchemist-goto-elixir-source-dir "~/Source/elixir")
(setq alchemist-goto-erlang-source-dir "~/Source/otp")

;; Add flychecker for dialyzer
(with-eval-after-load 'flycheck
  ;; Requires dogma globally
  ;; git clone https://github.com/lpil/dogma
  ;; cd dogma
  ;; mix archive.build
  ;; mix archive.install
  (flycheck-define-checker elixir-dogma
    "Defines a checker for elixir with dogma"
    :command ("mix" "dogma" "--format" "flycheck" "--stdin" source-original)
    :standard-input t
    :error-patterns
    (
     (info line-start (file-name) ":" line ":" column ": " (or "C" "R" "D") ": " (optional (id (one-or-more (not (any ":")))) ": ") (message) line-end)
     (warning line-start (file-name) ":" line ":" column ": " (or "W" "E" "F") ": " (optional (id (one-or-more (not (any ":")))) ": ") (message) line-end)
     )
    :modes (elixir-mode))

  (add-to-list 'flycheck-checkers 'elixir-dogma)

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
