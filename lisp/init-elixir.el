;; Elixir
;; Treat _ as a word character
(with-eval-after-load 'elixir-mode
  (modify-syntax-entry ?_ "w" elixir-mode-syntax-table))

(setq alchemist-test-ask-about-save nil)
(setq alchemist-goto-elixir-source-dir "~/Source/elixir")
(setq alchemist-goto-erlang-source-dir "~/Source/otp")

(defun elixir-flycheck-project-root (&rest _ignored)
  (and buffer-file-name
       (locate-dominating-file buffer-file-name "mix.exs")))

(with-eval-after-load 'flycheck
  (flycheck-define-checker elixir-dogma
    "Defines a checker for elixir with dogma"
    :command ("mix" "dogma" "--format" "flycheck" source)
    :predicate
    (lambda ()
      (file-exists-p "deps/dogma"))
    :error-patterns
    (
     (info line-start (file-name) ":" line ":" column ": " (or "C" "R" "D") ": " (optional (id (one-or-more (not (any ":")))) ": ") (message) line-end)
     (warning line-start (file-name) ":" line ":" column ": " (or "W" "E" "F") ": " (optional (id (one-or-more (not (any ":")))) ": ") (message) line-end))
    :working-directory elixir-flycheck-project-root
    :modes elixir-mode)

  (flycheck-define-checker elixir-dialyzer
    "Erlang syntax checker based on dialyzer."
    :command ("mix" "dialyzer" "--fullpath" "--no-check")
    :predicate
    (lambda ()
      (and
       (flycheck-buffer-saved-p)
       (file-exists-p "deps/dialyxir")))
    :error-patterns
    ((warning line-start
              (file-name)
              ":"
              line
              ": warning: "
              (message)
              line-end)
     (error line-start
            (file-name)
            ":"
            line
            ":"
            (message)
            line-end))
    :working-directory elixir-flycheck-project-root
    :modes elixir-mode)

  (flycheck-credo-setup)

  (add-to-list 'flycheck-checkers 'elixir-dogma)
  (add-to-list 'flycheck-checkers 'elixir-dialyzer t)
  (flycheck-add-next-checker 'elixir-dogma 'elixir-credo)
  (flycheck-add-next-checker 'elixir-credo 'elixir-dialyzer))

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
