(defvar-local company-simple-complete--previous-prefix nil)
(defvar-local company-simple-complete--before-complete-point nil)

(defun company-simple-complete/post-init-company ()
  (with-eval-after-load 'company
    (define-key company-active-map [tab] 'company-simple-complete/next)
    (define-key company-active-map (kbd "TAB") 'company-simple-complete/next)
    (define-key company-active-map (kbd "<S-tab>") 'company-simple-complete/previous)
    (define-key company-active-map (kbd "RET") nil)
    (define-key company-active-map (kbd "<return>") nil)

    (put 'company-simple-complete/next 'company-keep t)
    (put 'company-simple-complete/previous 'company-keep t)
    (ad-activate 'company-set-selection)
    (ad-activate 'company-tooltip--simple-update-offset)
    (ad-activate 'company-tooltip--lines-update-offset)
    (add-to-list 'company-frontends 'company-simple-complete//frontend)))
