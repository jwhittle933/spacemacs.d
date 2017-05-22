;;; packages.el --- aj-mail layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Aaron Jensen <aaronjensen@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

(defconst aj-mail-packages
  '(mu4e
    (window-margin :location local))
  "The list of Lisp packages required by the aj-mail layer.")

(defun aj-mail/post-init-mu4e ()
  ;; Mail contexts
  (setq mu4e-context-policy 'pick-first)
  (setq mu4e-compose-context-policy 'ask)

  ;; Indexing mail
  (setq mu4e-get-mail-command "/usr/local/bin/mbsync -a")
  (setq mu4e-update-interval (* 3 60))
  (setq mu4e-hide-index-messages t)
  (setq mu4e-index-lazy-check t)
  ;; NEEDED FOR MBSYNC
  (setq mu4e-change-filenames-when-moving t)

  ;; Index view
  (setq mu4e-confirm-quit nil)
  (setq mu4e-maildir-shortcuts
        '(("/inbox" . ?i)))

  ;; Header view
  (setq mu4e-headers-visible-lines 16)
  (setq mu4e-headers-fields
        '((:human-date . 12)
          (:flags . 6)
          (:from . 22)
          (:subject . nil)))
  ;; Gmail has duplicates between inbox and all mail, skip them
  (setq mu4e-headers-skip-duplicates t)

  ;; Viewing mail

  ;; This controls whether or not to assume html has more content than plaintext.
  ;; I bumped it up to get github notifications showing as plaintext, I may some
  ;; day want to disable it.
  (setq mu4e-view-html-plaintext-ratio-heuristic 10)

  ;; enable inline images
  (setq mu4e-view-show-images t
        mu4e-view-image-max-width 800)

  ;; (setq mu4e-html2text-command "/usr/local/bin/w3m -T text/html")
  ;; (setq mu4e-html2text-command "/usr/local/bin/html2text -width 72")
  ;; (setq mu4e-html2text-command "textutil -stdin -format html -convert txt -stdout")
  ;; (setq mu4e-html2text-command "html2text")
  (setq mu4e-html2text-command 'mu4e-shr2text)
  ;; (advice-add #'shr-colorize-region :around (defun shr-no-colourise-region (&rest ignore)))
  ;; (setq shr-color-visible-luminance-min 40)

  ;; Composing mail

  (setq mu4e-compose-in-new-frame t)
  ;; enable format=flowed
  (setq mu4e-compose-format-flowed t)
  (add-hook 'mu4e-view-mode-hook 'visual-line-mode)
  (add-hook 'mu4e-view-mode-hook 'turn-on-window-margin-mode)

  (add-hook 'mu4e-compose-mode-hook (lambda () (auto-fill-mode -1)))
  (add-hook 'mu4e-compose-mode-hook 'turn-on-window-margin-mode)

  ;; Sending mail
  (setq message-send-mail-function 'smtpmail-send-it)
  (setq message-kill-buffer-on-exit t)
  (setq mail-user-agent 'mu4e-user-agent)

  ;; org integration
  ;; store link to message if in header view
  (setq org-mu4e-link-query-in-headers-mode nil)

  ;; use imagemagick, if available
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))

  (with-eval-after-load 'mu4e
    (advice-add 'mu4e-mark-execute-all :after 'aj-mail/mu4e-push)
    (define-key mu4e-headers-mode-map (kbd "s-r") 'aj-mail/mu4e-refresh-inbox)))

(defun aj-mail/init-window-margin ()
  (use-package window-margin
    :defer t
    :commands
    (turn-on-window-margin-mode
     turn-off-window-margin-mode
     window-margin-mode)))
;;; packages.el ends here
