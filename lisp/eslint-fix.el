;; Runs eslint --fix after save
(defun eslint-fix ()
  (interactive)
  (let ((current-point (point))
        (line (count-screen-lines (window-start) (point)))
        (command (concat
                  flycheck-javascript-eslint-executable
                  " --stdin"
                  " --fix-to-stdout"
                  " --stdin-filename " buffer-file-name))
        (buffer (current-buffer))
        (text (buffer-substring-no-properties (point-min) (point-max))))
    (with-temp-buffer
      (insert text)
      (when (eq 0
                (shell-command-on-region
                 ;; Region
                 (point-min)
                 (point-max)
                 ;; Command
                 command
                 ;; Output to current buffer
                 t
                 ;; Replace buffer
                 t
                 ;; Error buffer name
                 "*eslint-fix error*"))
        (let ((fixed-text (buffer-substring-no-properties (point-min) (point-max))))
          (with-current-buffer buffer
            (unless (or
                     (string= fixed-text
                              (buffer-substring-no-properties (point-min) (point-max)))
                     (s-starts-with-p "Error:" fixed-text ))

              (delete-region (point-min) (point-max))
              (insert fixed-text)
              ;; Restore point and scroll position
              (goto-char current-point)
              (recenter (- line 1)))))))))

(provide 'eslint-fix)
