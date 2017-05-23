(defun aj-mail/enable-mu4e-gmail-support ()
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

(defun aj-mail/persp-mu4e ()
  "Open mu4e in its own persp and switch to it then refresh inbox."
  (interactive)
  (persp-switch "@Mu4e")
  (unless (get-buffer-window "*mu4e-headers*")
    (mu4e t)
    (mu4e-headers-search-bookmark
     (mu4e-get-bookmark-query ?i))
    (delete-other-windows))
  (aj-mail/mu4e-refresh-inbox t))

(defun aj-mail/mu4e-refresh-inbox (&rest background)
  "Refresh only mbsync inbox"
  (interactive)
  (let ((mu4e-get-mail-command "/usr/local/bin/mbsync inbox"))
    (mu4e-update-mail-and-index background)))

(defun aj-mail/mu4e-push (&rest ignored)
  "Push email changes."
  (interactive)
  (let ((mu4e-get-mail-command "/usr/local/bin/mbsync -a --push"))
    (mu4e-update-mail-and-index t)))
