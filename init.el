(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     better-defaults
     spacemacs-layouts
     ivy
     ;; helm
     (markdown :variables
               markdown-live-preview-engine 'vmd)
     (syntax-checking :variables
                      syntax-checking-enable-tooltips nil)
     (auto-completion :variables
                      auto-completion-enable-sort-by-usage nil
                      auto-completion-enable-snippets-in-popup nil
                      ;; spacemacs adds .emacs.d/private/snippets as the first snippet dir
                      ;; but I want to save to .spacemacs.d by default.
                      auto-completion-private-snippets-directory
                      (expand-file-name
                       "snippets"
                       dotspacemacs-directory))
     git
     dash
     pdf-tools
     html
     clojure
     (org :variables
          org-enable-github-support t
          org-enable-reveal-js-support t)
     colors
     osx
     ;; vinegar
     github
     ruby
     (shell :variables
            shell-default-shell 'ansi-term
            shell-default-height 30
            shell-default-position 'bottom)
     (spell-checking :variables
                     spell-checking-enable-by-default nil)
     ranger
     version-control
     rcirc
     tmux
     yaml
     docker
     elm
     restclient
     typescript
     lua

     ;; TheBB's layers
     ;; https://github.com/TheBB/spacemacs-layers
     ;; no-dots
     evil-little-word

     ;; Personal layers
     aj-deft
     aj-elixir
     aj-emacs-lisp
     aj-javascript
     auto-correct
     cleverparens-lispy
     contextual-menubar
     editorconfig
     fix-git-autorevert
     flow
     frame-geometry
     match-indent
     shift-number
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages
   '(
     evil-terminal-cursor-changer
     company-flx
     graphviz-dot-mode
     org-gcal
     org-mobile-sync
     xclip
     )
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages
   '(
     magithub
     )
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and uninstall any
   ;; unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't uninstall
   ;; them if they become unused. `all' installs *all* packages supported by
   ;; Spacemacs and never uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory 'emacs-version
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style '(hybrid :variables
                                       hybrid-mode-default-state 'normal
                                       hybrid-mode-enable-evilified-state t
                                       hybrid-mode-enable-hjkl-bindings nil)
   ;; If non-nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner nil
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(monokai
                         whiteboard
                         solarized-light)
   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Inconsolatag"
                               :size 14
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key ":"
   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab t
   ;; If non-nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ t
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non-nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non-nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non-nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non-nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non-nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil
   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar nil
   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native t
   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non-nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols nil
   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; If non-nil line numbers are turned on in all `prog-mode' and `text-mode'
   ;; derivatives. If set to `relative', also turns on relative line numbers.
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'changed
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init'.  You are free to put almost any
user code here.  The exception is org related code, which should be placed in
`dotspacemacs/user-config'."
  (add-to-list 'load-path (expand-file-name "lisp" dotspacemacs-directory))
  (require 'init-evil)
  (require 'init-html)

  (setq exec-path-from-shell-arguments '("-l"))
  (add-to-list 'auto-mode-alist '("\\.?\\(bashrc\\|zshrc\\|shellrc\\|bash_profile\\)" . sh-mode))
  (add-to-list 'auto-mode-alist '("\\.?\\(eslintrc\\)" . json-mode))

  (setq winum-scope 'frame-local)
  (setq frame-resize-pixelwise t)
  (setq create-lockfiles nil)
  (setq require-final-newline t)
  (setq inhibit-compacting-font-caches t)
  ;; TODO: Remove after Emacs 26.1, it is the default there
  (setq switch-to-buffer-preserve-window-point t)

  ;; smartparens
  (setq sp-highlight-pair-overlay nil)

  (setq flycheck-display-errors-delay 0.5)

  ;; Ruby
  ;; Treat _ as a word character
  (with-eval-after-load 'ruby-mode
    (modify-syntax-entry ?_ "w" ruby-mode-syntax-table))

  ;; Company
  ;; Fuzzy completion
  ;; (with-eval-after-load 'company
  ;;   (company-flx-mode +1))
  ;; Speed up autocomplete popup
  (setq company-idle-delay 0.1)

  ;; RCIRC
  ;; Keep line at margin-bottom: ...
  (add-hook 'rcirc-mode-hook
            (lambda ()
              (set (make-local-variable 'scroll-conservatively) 8192)))
  (add-hook 'before-make-frame-hook
            (lambda ()
              (unless window-system
                (menu-bar-mode -1))))
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration. You are free to put any user code."
  (require 'init-magit)
  (require 'init-org)
  (require 'init-terminal-cursor)
  (require 'init-flyspell)
  (require 'init-typescript)
  (require 'init-xclip)

  (require 'company-simple-complete)
  (require 'fill-or-unfill)
  (require 'quiet-emacs)

  ;; Delete consecutive dupes from company in case they differ by annotation only
  ;; https://github.com/company-mode/company-mode/issues/528
  (with-eval-after-load 'company
    (add-to-list 'company-transformers 'delete-consecutive-dups t))

  (spacemacs|do-after-display-system-init
   (setq powerline-default-separator 'alternate)
   (setq spaceline-version-control-p nil)
   (spaceline-compile))

  ;; Indentation
  (setq
   sh-basic-offset 2
   sh-indentation 2
   css-indent-offset 2
   )

  ;; Prevent persp from loading existing perspectives when opening new frames.
  ;; This fixes a flash of another buffer when opening things from the terminal.
  ;; https://github.com/Bad-ptr/persp-mode.el/issues/64
  ;; https://github.com/Bad-ptr/persp-mode.el/issues/36
  (setq persp-emacsclient-init-frame-behaviour-override nil)
  (setq dtrt-indent-max-merge-deviation 9.0)

  ;; ivy
  ;; Use fuzzy finder
  (setq ivy-re-builders-alist
        '((swiper . ivy--regex-plus)
          (t . ivy--regex-fuzzy)))
  ;; Do not insert ^
  (setq ivy-initial-inputs-alist nil)

  ;; Add `M-o v' and `M-o s' to open projectile files and buffers in splits
  ;; from ivy
  (with-eval-after-load 'counsel-projectile
    (ivy-set-actions
     'counsel-projectile-find-file
     '(("v" aj/projectile-find-file-vsplit "in vertical split")
       ("s" aj/projectile-find-file-split "in horizontal split")
       ("d" aj/projectile-delete-file-confirm "delete file"))))
  (ivy-set-actions
   'ivy-switch-buffer
   '(("v" aj/pop-to-buffer-vsplit "in vertical split")
     ("s" aj/pop-to-buffer-split "in horizontal split")))
  ;; Add i and w to ivy actions to insert/copy the selection
  (ivy-set-actions
   t
   '(("i" aj/ivy-insert "insert")
     ("w" aj/ivy-kill-new "copy")))
  (ivy-set-actions
   'spacemacs/counsel-search
   spacemacs--ivy-grep-actions)

  (defun aj/projectile-find-file-split (file)
    (spacemacs/find-file-split (expand-file-name file (projectile-project-root))))
  (defun aj/projectile-find-file-vsplit (file)
    (spacemacs/find-file-vsplit (expand-file-name file (projectile-project-root))))
  (defun aj/projectile-delete-file-confirm (file)
    (spacemacs/delete-file-confirm (expand-file-name file (projectile-project-root))))
  (defun aj/pop-to-buffer-vsplit (buffer)
    (pop-to-buffer buffer '(spacemacs//display-in-split (split-side . right))))
  (defun aj/pop-to-buffer-split (buffer)
    (pop-to-buffer buffer '(spacemacs//display-in-split (split-side . below))))
  (defun aj/ivy-insert (x)
    (insert
     (if (stringp x)
         x
       (car x))))
  (defun aj/ivy-kill-new (x)
    (kill-new
     (if (stringp x)
         x
       (car x))))

  ;; Enable /sudo:root@server:
  (add-to-list 'tramp-default-proxies-alist '(".*" "\\`root\\'" "/ssh:%h:"))

  (setq
   ;; Use bash because it's faster
   shell-file-name "/bin/bash"

   ;; Spaceline
   spaceline-minor-modes-p nil

   ;; File name completion
   read-file-name-completion-ignore-case t
   read-buffer-completion-ignore-case t

   ;; Miscellaneous
   vc-follow-symlinks t
   require-final-newline t

   ;; Enable midnight-mode to clean old buffers every day
   midnight-mode t
   )

  ;; Key Bindings
  (global-set-key (kbd "M-]") 'sp-slurp-hybrid-sexp)
  (global-set-key (kbd "C-x C-l") 'evil-complete-next-line)

  (spacemacs/set-leader-keys "SPC" 'avy-goto-char-2)
  (setq avy-timeout-seconds 0.2)

  (spacemacs/set-leader-keys "fel" 'counsel-find-library)

  ;; Profiler bindings
  (defun profiler-start-cpu ()
    (interactive)
    (profiler-start 'cpu))
  (spacemacs/set-leader-keys "ops" 'profiler-start-cpu)
  (spacemacs/set-leader-keys "opr" 'profiler-report)
  (spacemacs/set-leader-keys "opt" 'profiler-stop)
  (spacemacs/set-leader-keys "opx" 'profiler-reset)
  (spacemacs/set-leader-keys "oper" 'elp-results)

  ;; Bury buffers instead of killing them by default
  (spacemacs/set-leader-keys "bd" 'bury-buffer)
  (spacemacs/set-leader-keys "bk" 'spacemacs/kill-this-buffer)
  (spacemacs/set-leader-keys "bK" 'spacemacs/kill-other-buffers)

  ;; Use C-j in place of C-x
  ;; (define-key key-translation-map "\C-j" "\C-x")
  (global-set-key (kbd "<s-return>") 'spacemacs/toggle-fullscreen-frame)

  ;; Word wrap in text buffers
  (add-hook 'text-mode-hook 'auto-fill-mode)

  ;; Don't copy text to system clipboard while selecting it
  (fset 'evil-visual-update-x-selection 'ignore)

  ;; Remap paste to be able to paste multiple times
  ;; If I don't like this, maybe I'll try this:
  ;; https://github.com/Dewdrops/evil-ReplaceWithRegister/
  (defun evil-paste-after-from-0 ()
    (interactive)
    (let ((evil-this-register ?0))
      (call-interactively 'evil-paste-after)))

  (define-key evil-visual-state-map "p" 'evil-paste-after-from-0)

  (dotimes (n 10)
    ;; Map s-<number> to switch layouts
    (global-set-key (kbd (format "s-%d" n)) (intern (format "spacemacs/persp-switch-to-%d" n)))
    ;; Map M-<number> to workspace switching
    (let ((key (kbd (format "M-%d" n))))
      (define-key winum-keymap key nil)
      (global-unset-key key)
      (global-set-key key (intern (format "eyebrowse-switch-to-window-config-%d" n)))))

  ;; Prevent font size changes from resizing frame
  (setq frame-inhibit-implied-resize t)
  ;; Change entire frame font size
  (defun my-alter-frame-font-size (fn)
    (let* ((current-font-name (frame-parameter nil 'font))
           (decomposed-font-name (x-decompose-font-name current-font-name))
           (font-size (string-to-int (aref decomposed-font-name 5))))
      (aset decomposed-font-name 5 (int-to-string (funcall fn font-size)))
      (set-frame-font (x-compose-font-name decomposed-font-name))))

  (defun my-inc-frame-font-size ()
    (interactive)
    (my-alter-frame-font-size '1+))

  (defun my-dec-frame-font-size ()
    (interactive)
    (my-alter-frame-font-size '1-))

  (global-set-key (kbd "s-+") 'my-inc-frame-font-size)
  (global-set-key (kbd "s-=") 'my-inc-frame-font-size)
  (global-set-key (kbd "s--") 'my-dec-frame-font-size)
  (global-set-key (kbd "C-+") 'spacemacs/scale-up-font)
  (global-set-key (kbd "C-=") 'spacemacs/scale-up-font)
  (global-set-key (kbd "C--") 'spacemacs/scale-down-font)

  ;; Pairing stuff
  (global-set-key (kbd "<end>") 'evil-end-of-line)

  ;; load private settings
  (when (file-exists-p "~/.emacs-private.el")
    (load-file "~/.emacs-private.el"))
  )

(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(max-specpdl-size 2000)
 '(package-selected-packages
   (quote
    (string-inflection symon xclip browse-at-remote lua-mode gnuplot-mode wgrep smex rjsx-mode ivy-purpose ivy-hydra flyspell-correct-ivy counsel-dash org-mobile-sync shift-number eslintd-fix vmd-mode request-deferred deferred org-gcal solarized-theme fuzzy magithub flycheck-dogma flycheck-dialyxir pdf-tools winum counsel-projectile counsel unfill wgrep-ag eros vimrc-mode dactyl-mode nameless evil-multiedit add-node-modules-path org-tree-slide ox-reveal restclient-helm ob-restclient company-restclient know-your-http-well hide-comnt package-lint ob-elixir helm-purpose window-purpose imenu-list minitest pug-mode tide typescript-mode restclient ob-http zoutline parent-mode goto-chg undo-tree diminish flx seq spinner bind-key pkg-info epl flycheck-credo flycheck-package osx-dictionary company-flow dumb-jump ht flycheck-flow helm-gtags ggtags emoji-cheat-sheet-plus editorconfig company-emoji org marshal flycheck-mix evil-unimpaired popup evil-terminal-cursor-changer org-projectile mwim github-search flycheck-elm elm-mode yaml-mode dockerfile-mode docker tablist docker-tramp flyspell-correct-helm anzu highlight ox-gfm color-identifiers-mode flyspell-correct align-cljlet iedit nlinum-relative nlinum bind-map dash evil-visual-mark-mode ruby-end s ivy async hydra org-download projectile smartparens helm helm-core avy package-build evil eyebrowse column-enforce-mode clojure-snippets clj-refactor inflections edn peg cider-eval-sexp-fu cider queue clojure-mode evil-cleverparens paredit xterm-color web-mode web-beautify toc-org tagedit stickyfunc-enhance srefactor spaceline powerline smeargle slim-mode shell-pop shackle scss-mode sass-mode rvm ruby-tools ruby-test-mode rubocop rspec-mode robe reveal-in-osx-finder rcirc-notify rcirc-color rbenv ranger rake rainbow-mode rainbow-identifiers pbcopy osx-trash orgit org-repo-todo org-present org-pomodoro alert log4e gntp org-plus-contrib org-bullets multi-term mmm-mode markdown-toc markdown-mode magit-gitflow magit-gh-pulls macrostep livid-mode skewer-mode simple-httpd lispy swiper less-css-mode launchctl json-mode json-snatcher json-reformat js2-refactor multiple-cursors js2-mode js-doc jade-mode htmlize helm-gitignore request helm-flyspell helm-dash helm-css-scss helm-company helm-c-yasnippet haml-mode graphviz-dot-mode gnuplot gitignore-mode github-clone github-browse-file gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter gist gh logito pcache gh-md flycheck-pos-tip flycheck floobits evil-magit magit magit-popup git-commit with-editor eshell-z eshell-prompt-extras esh-help erlang emmet-mode elisp-slime-nav dtrt-indent diff-hl deft dash-at-point company-web web-completion-data company-tern dash-functional tern company-statistics company-quickhelp pos-tip company-flx coffee-mode chruby bundler inf-ruby auto-yasnippet yasnippet auto-dictionary auto-compile packed alchemist company elixir-mode ac-ispell auto-complete ws-butler window-numbering which-key volatile-highlights vi-tilde-fringe uuidgen use-package spacemacs-theme smooth-scrolling restart-emacs rainbow-delimiters quelpa popwin persp-mode pcre2el paradox page-break-lines open-junk-file neotree move-text monokai-theme lorem-ipsum linum-relative link-hint leuven-theme info+ indent-guide ido-vertical-mode hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido fill-column-indicator fancy-battery f expand-region exec-path-from-shell evil-visualstar evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu define-word clean-aindent-mode buffer-move bracketed-paste auto-highlight-symbol aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line)))
 '(paradox-github-token t)
 '(safe-local-variable-values (quote ((create-lockfiles . t))))
 '(send-mail-function (quote mailclient-send-it)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(avy-lead-face ((t (:background "#20211c" :foreground "#F92672"))))
 '(avy-lead-face-0 ((t (:background "#20211c" :foreground "#e5236a"))))
 '(avy-lead-face-1 ((t (:background "#20211c" :foreground "#d32062"))))
 '(avy-lead-face-2 ((t (:background "#20211c" :foreground "#c41e5b"))))
 '(aw-leading-char-face ((t (:foreground "#F92672" :height 8.0))))
 '(diff-refine-added ((t (:background "#394e10" :foreground "#A6E22E"))))
 '(diff-refine-removed ((t (:background "#430b1e" :foreground "#F92672"))))
 '(sp-show-pair-match-face ((t (:background "#AE81FF" :foreground "#272822" :inverse-video nil :weight normal)))))
)
