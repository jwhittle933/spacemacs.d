(defun company-simple-complete//frontend (command)
  (when (or (eq command 'show)
            (and (eq command 'update)
                 (not (equal company-prefix company-simple-complete--previous-prefix))))
    (setq company-selection 0
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
        (let ((result (nth company-selection company-candidates)))
          (when company-simple-complete--before-complete-point
            (delete-region company-simple-complete--before-complete-point (point)))
          (setq company-simple-complete--before-complete-point (point))
          (unless (eq result company-simple-complete--no-selection)
            (company--insert-candidate result))
          (company-call-frontends 'update)
          (company-call-frontends 'post-command)))
    (company-complete-selection)))

(defadvice company--create-lines (after remove-no-selection ())
  "Remove the company no selection placeholder"
  (let ((first (car ad-return-value)))
    (when (and first
               (equal company-simple-complete--no-selection
                      (string-trim (substring-no-properties first))))
      (setq ad-return-value (cdr ad-return-value)))))

(defun company-simple-complete//add-no-selection (candidates)
  (if (cdr candidates)
      (push company-simple-complete--no-selection candidates)
    candidates))
