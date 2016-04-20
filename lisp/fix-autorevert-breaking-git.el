;; Unaccepted fix for http://debbugs.gnu.org/cgi/bugreport.cgi?bug=21559
;; Based on https://github.com/bgamari/emacs/commit/64c97e7d123e7796a204c640de8ad5e03c9c9cc0
;; Thanks Ben Gamari!

(with-eval-after-load 'autorevert
  (defvar auto-revert-vc-check-timer nil
    "Timer used by Auto-Revert Mode to schedule
checks of version control information. See
`auto-revert-schedule-vc-check' for details.")

  (defcustom auto-revert-vc-check-idle-time 2
    "How much time to wait after noticing a changed file before calling
`vc-find-file-hook' or nil to check immediately."
    :group 'auto-revert
    :type 'number
    :version "25.1")

  (defun auto-revert-schedule-vc-check ()
    "Schedule a call to `vc-find-file-hook'.

We need to be careful when calling `vc-refresh-state' after file changes
as some version control utilities (e.g. git rebase) have a tendency
to do many successive calls and will fail ungracefully if they find
we have taken the repository lock.

For this reason we wait for the repository to be idle for at least
`auto-revert-vc-check-idle-time' seconds before calling
`vc-refresh-state'."
    (if auto-revert-vc-check-idle-time
        (progn
          (when (timerp auto-revert-vc-check-timer)
            (cancel-timer auto-revert-vc-check-timer))
          (setq auto-revert-vc-check-idle-timer
                (run-at-time auto-revert-vc-check-idle-time nil
                             'vc-find-file-hook))
          )
      (vc-refresh-state)))

  (defadvice auto-revert-handler (around refresh-on-idle ())
    (flet ((vc-refresh-state () (auto-revert-schedule-vc-check)))
      ad-do-it))
  (ad-activate 'auto-revert-handler))

(provide 'fix-autorevert-breaking-git)
