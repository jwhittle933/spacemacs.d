(require 'evil-terminal-cursor-changer)
(defun etcc--make-iterm-tmux-shape-seq (shape)
  (let ((prefix "\e[")
        (suffix " q")
        (box "0")
        (bar "3"))
        (case shape
                ('box (concat prefix box suffix))
                ('bar (concat prefix bar suffix)))))

(defadvice etcc--make-cursor-shape-seq (around iterm-with-tmux (shape))
  ""
  (if (and (etcc--in-iterm?)
           (etcc--in-tmux?))
      (setq ad-return-value (etcc--make-iterm-tmux-shape-seq shape))
    ad-do-it))

(ad-activate 'etcc--make-cursor-shape-seq)
(provide 'init-terminal-cursor)
