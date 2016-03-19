(defun company-simple-complete//frontend (command)
  (when (or (eq command 'show)
            (and (eq command 'update)
                 (not (equal company-prefix company-simple-complete--previous-prefix))))
    (setq company-selection -1
          company-simple-complete--previous-prefix company-prefix
          company-simple-complete--before-complete-point nil)))

(defun company-simple-complete/next (&optional arg)
  (interactive "p")
  (company-select-next arg)
  (company-simple-complete//complete-selection-and-stay))

(defun company-simple-complete/previous (&optional arg)
  (interactive "p")
  (company-select-previous arg)
  (company-simple-complete//complete-selection-and-stay))

(defun company-simple-complete//complete-selection-and-stay ()
  (if (cdr company-candidates)
      (when (company-manual-begin)
        (when company-simple-complete--before-complete-point
          (delete-region company-simple-complete--before-complete-point (point)))
        (setq company-simple-complete--before-complete-point (point))
        (unless (eq company-selection -1)
          (company--insert-candidate (nth company-selection company-candidates)))
        (company-call-frontends 'update)
        (company-call-frontends 'post-command))
    (company-complete-selection)))

(defadvice company-set-selection (around allow-no-selection (selection &optional force-update))
  "Allow selection to be -1"
  (setq selection
        ;; TODO deal w/ wrap-around
        (if company-selection-wrap-around
            (mod selection company-candidates-length)
          (max -1 (min (1- company-candidates-length) selection))))
  (when (or force-update (not (equal selection company-selection)))
    (setq company-selection selection
          company-selection-changed t)
    (company-call-frontends 'update)))

(defadvice company-tooltip--lines-update-offset (before allow-no-selection (selection _num-lines _limit))
  "Allow selection to be -1"
  (when (eq selection -1)
    (ad-set-arg 0 0)))

(defadvice company-tooltip--simple-update-offset (before allow-no-selection (selection _num-lines limit))
  "Allow selection to be -1"
  (when (eq selection -1)
    (ad-set-arg 0 0)))
