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
(setq message-fill-column nil)
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

(defun remove-nth-element (nth list)
  (if (zerop nth) (cdr list)
    (let ((last (nthcdr (1- nth) list)))
      (setcdr last (cddr last))
      list)))

(defun aj/enable-mu4e-gmail-support ()
  "Make changes necessary to support gmail. Must call after mu4e is loaded."
  ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
  (setq mu4e-sent-messages-behavior 'delete)

  ;; Avoid trashing when deleting--gmail treats the trash as a normal folder
  ;; and mbsync is not configured to expunge any way
  (add-to-list 'mu4e-marks
               '(trash
                 :char ("d" . "▼")
                 :prompt "dtrash"
                 :dyn-target (lambda (target msg) (mu4e-get-trash-folder msg))
                 :action (lambda (docid msg target)
                           (mu4e~proc-move docid
                                           (mu4e~mark-check-target target) "-N")))))

;; use imagemagick, if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

(use-package window-margin
  :defer t
  :commands
  (turn-on-window-margin-mode
   turn-off-window-margin-mode
   window-margin-mode))

(defun aj/persp-mu4e ()
  (interactive)
  (persp-switch "@Mu4e")
  (unless (get-buffer-window "*mu4e-headers*")
    (mu4e-headers-search-bookmark
     (mu4e-get-bookmark-query ?i))
    (delete-other-windows))
  (aj/mu4e-refresh-inbox t))
(global-set-key (kbd "s-0") 'aj/persp-mu4e)

(defun aj/mu4e-refresh-inbox (&rest background)
  "Refresh only mbsync inbox"
  (interactive)
  (let ((mu4e-get-mail-command "/usr/local/bin/mbsync inbox"))
    (mu4e-update-mail-and-index background)))
(define-key mu4e-headers-mode-map (kbd "s-r") 'aj/mu4e-refresh-inbox)

(defun aj/mu4e-push (&rest ignored)
  "Push email changes."
  (interactive)
  (let ((mu4e-get-mail-command "/usr/local/bin/mbsync -a --push"))
    (mu4e-update-mail-and-index t)))

(with-eval-after-load 'mu4e-mark
  (advice-add 'mu4e-mark-execute-all :after 'aj/mu4e-push))

(provide 'init-mail)
