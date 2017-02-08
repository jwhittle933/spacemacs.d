(defconst hub-packages '(magithub))

(defun hub/init-magithub ()
  (use-package magithub
    :after magit
    :config (magithub-feature-autoinject t)))
