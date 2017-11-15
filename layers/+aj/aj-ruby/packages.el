;;; packages.el --- aj-ruby layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Aaron Jensen <aaronjensen@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `aj-ruby-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `aj-ruby/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `aj-ruby/pre-init-PACKAGE' and/or
;;   `aj-ruby/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst aj-ruby-packages
  '(ruby-mode
    ruby-test-mode))

(defun aj-ruby/post-init-ruby-mode ()
  (add-hook 'ruby-mode-hook #'aj-ruby/rubocop-set-flycheck-executable))

(defun aj-ruby/post-init-ruby-test-mode ()
  (with-eval-after-load 'ruby-test-mode
    (advice-add 'ruby-test-run-at-point :before (lambda (&rest r) (save-buffer)))
    (advice-add 'ruby-test-run :before (lambda (&rest r) (save-buffer))))
  (setq ruby-test-rspec-options '()))
;;; packages.el ends here
