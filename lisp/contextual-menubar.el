;; http://emacs.stackexchange.com/questions/29441/how-do-i-disable-menu-bar-mode-only-for-tty-frames

(defun contextual-menubar (&optional frame)
  "Display the menubar in FRAME (default: selected frame) if on a
    graphical display, but hide it if in terminal."
  (interactive)
  (set-frame-parameter frame 'menu-bar-lines
                       (if (display-graphic-p frame)
                           1 0)))

(add-hook 'after-make-frame-functions 'contextual-menubar)

(provide 'contextual-menubar)
