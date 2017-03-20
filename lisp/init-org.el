(defun org-keys ()
  (interactive)
  ;; Make ~SPC ,~ work, reference:
  ;; http://stackoverflow.com/questions/24169333/how-can-i-emphasize-or-verbatim-quote-a-comma-in-org-mode
  (setcar (nthcdr 2 org-emphasis-regexp-components) " \t\n")
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)

  (setq org-emphasis-alist '(("*" bold)
                             ("/" italic)
                             ("_" underline)
                             ("=" org-verbatim verbatim)
                             ("~" org-kbd)
                             ("+"
                              (:strike-through t))))

  (setq org-hide-emphasis-markers t))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "M-h") 'org-metaleft)
  (define-key org-mode-map (kbd "M-j") 'org-metadown)
  (define-key org-mode-map (kbd "M-k") 'org-metaup)
  (define-key org-mode-map (kbd "M-l") 'org-metaright)
  (define-key org-mode-map (kbd "M-H") 'org-shiftmetaleft)
  (define-key org-mode-map (kbd "M-J") 'org-shiftmetadown)
  (define-key org-mode-map (kbd "M-K") 'org-shiftmetaup)
  (define-key org-mode-map (kbd "M-L") 'org-shiftmetaright)
  (org-keys))

(spacemacs/set-leader-keys "bo" 'org-iswitchb)

(setq org-mobile-force-id-on-agenda-items nil)
(setq org-startup-indented t)
(setq org-agenda-todo-ignore-scheduled t)
(setq org-agenda-todo-ignore-deadlines t)
(setq org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))
(setq org-agenda-sticky t)

(add-hook 'org-capture-mode-hook 'evil-insert-state)

;; Refresh calendars via org-gcal and automatically create appt-reminders.
;; Appt will be refreshed any time an org file is saved after 10 seconds of idle.
;; gcal will be synced after 1 minute of idle every 15 minutes.
;; Start with `(aj-sync-calendar-start)'
(defvar aj-refresh-appt-timer nil
  "Timer that `aj-refresh-appt-with-delay' uses to reschedule itself, or nil.")
(defun aj-refresh-appt-with-delay ()
  (when aj-refresh-appt-timer
    (cancel-timer aj-refresh-appt-timer))
  (setq aj-refresh-appt-timer
        (run-with-idle-timer
         10 nil
         (lambda ()
           (setq appt-time-msg-list nil)
           (org-agenda-to-appt)
           (message nil)))))

(defvar aj-sync-calendar-timer nil
  "Timer that `aj-sync-calendar-with-delay' uses to reschedule itself, or nil.")
(defun aj-sync-calendar-with-delay ()
  (when aj-sync-calendar-timer
    (cancel-timer aj-sync-calendar-timer))
  (setq aj-sync-calendar-timer
        (run-with-idle-timer
         60 nil
         (lambda ()
           (org-gcal-refresh-token)
           (org-gcal-fetch)))))

(defun aj-sync-calendar-start ()
  (add-hook 'after-save-hook
            (lambda ()
              (when (eq major-mode 'org-mode)
                (aj-refresh-appt-with-delay))))

  (run-with-timer
   0 (* 15 60)
   'aj-sync-calendar-with-delay))

(advice-add 'org-agenda-quit :before 'org-save-all-org-buffers)
(advice-add 'org-agenda-todo :after 'org-save-all-org-buffers)
(advice-add 'org-agenda-deadline :after 'org-save-all-org-buffers)
(advice-add 'org-agenda-schedule :after 'org-save-all-org-buffers)

;; Custom org-agenda view
(setq org-agenda-compact-blocks t)
(setq org-agenda-custom-commands
      (quote ((" " "Agenda"
               ((agenda "" ((org-agenda-span 'day)))
                (todo ""))))))
(defun org-agenda-show-agenda (&optional arg)
  (interactive "P")
  (org-agenda arg " ")
  (org-agenda-redo))
(spacemacs/set-leader-keys "oa" 'org-agenda-show-agenda)

;; org-refile settings
(setq org-refile-targets '((nil :maxlevel . 9)
                           (org-agenda-files :maxlevel . 9)))
(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)

(defun aj/verify-refile-target ()
  "Exclude todo keywords with a done state and org-gcal files"
  (and (not (member (nth 2 (org-heading-components)) org-done-keywords))
       (not (member buffer-file-name (mapcar #'cdr org-gcal-file-alist)))))

(setq org-refile-target-verify-function 'aj/verify-refile-target)

;; org-capture settings
(setq org-capture-templates
      '(("n" "Notes" entry
         (file (concat org-directory "/refile.org"))
         "* %? :NOTE:\n%U\n%a")
        ("t" "Todo" entry
         (file+headline (concat org-directory "/todo.org") "To Do")
         "* TODO %?\n%U\n%a")
        ("s" "Scheduled Task" entry
         (file+headline (concat org-directory "/schedule.org") "Tasks")
         "* TODO %?\nSCHEDULED: %^{When}t")))

(setq org-projectile:capture-template "* TODO %?\n%U\n%a")

(provide 'init-org)
