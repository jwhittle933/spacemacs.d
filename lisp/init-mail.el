;; Mail contexts
(setq mu4e-context-policy 'pick-first)
(setq mu4e-compose-context-policy 'ask)

;; Indexing mail
(setq mu4e-get-mail-command "/usr/local/bin/mbsync -aq")
(setq mu4e-update-interval (* 3 60))
(setq mu4e-hide-index-messages t)
(setq mu4e-index-lazy-check t)
;; NEEDED FOR MBSYNC
(setq mu4e-change-filenames-when-moving t)

;; Index view
(setq mu4e-maildir-shortcuts
      '(("/inbox" . ?i)))

;; Header view
(setq mu4e-headers-fields
      '((:human-date . 12)
        (:flags . 6)
        (:from . 22)
        (:subject . nil)))

;; Viewing mail

(with-eval-after-load 'mu4e
  (add-to-list 'mu4e-view-actions
               '("ViewInBrowser" . mu4e-action-view-in-browser) t))

;; enable inline images
(setq mu4e-view-show-images t
      mu4e-view-image-max-width 800)

;; (setq mu4e-html2text-command "/usr/local/bin/w3m -T text/html")
;; (setq mu4e-html2text-command "/usr/local/bin/html2text -width 72")
(setq mu4e-html2text-command "textutil -stdin -format html -convert txt -stdout")
;; (setq mu4e-html2text-command "html2text")
;; (setq mu4e-html2text-command 'mu4e-shr2text)
;; (advice-add #'shr-colorize-region :around (defun shr-no-colourise-region (&rest ignore)))
;; (setq shr-color-visible-luminance-min 40)

;; Composing mail

(setq mu4e-compose-in-new-frame t)
;; enable format=flowed
(setq mu4e-compose-format-flowed t)
(setq message-fill-column nil)
(add-hook 'mu4e-view-mode-hook 'visual-line-mode)

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
  (setq mu4e-marks (remove-nth-element 5 mu4e-marks))
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

(provide 'init-mail)
