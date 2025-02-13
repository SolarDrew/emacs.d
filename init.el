;; Kickstart.emacs is *not* a distribution.
;; It's a template for your own configuration.

;; It is *recommeded* to configure it from the *config.org* file.
;; The goal is that you read every line, top-to-bottom, understand
;; what your configuration is doing, and modify it to suit your needs.

;; You can delete this when you're done. It's your config now. :)

;; The default is 800 kilobytes. Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(defun start/org-babel-tangle-config ()
  "Automatically tangle our Emacs.org config file when we save it. Credit to Emacs From Scratch for this one!"
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name user-emacs-directory))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'start/org-babel-tangle-config)))

(require 'use-package-ensure) ;; Load use-package-always-ensure
(setq use-package-always-ensure t) ;; Always ensures that a package is installed
(setq package-archives '(("melpa" . "https://melpa.org/packages/") ;; Sets default package repositories
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/"))) ;; For Eat Terminal

;;(use-package quelpa)
;;(use-package quelpa-use-package)

(use-package general
  :config
  (general-evil-setup)
  ;; Global keys
  (general-define-key
   :states '(normal visual motion emacs)
   "K" 'scroll-down-command
   "J" 'scroll-up-command
   )

  ;; Set up a local-leader
  (general-create-definer my-local-leader
    :prefix ","
    )
  (my-local-leader
   :states 'normal
   "a" 'ranger
   )
  ;; Set up 'SPC' as the leader key
  (general-create-definer start/leader-keys
    :states '(normal insert visual motion emacs)
    :keymaps 'override
    :prefix "SPC"           ;; Set leader key
    :global-prefix "C-SPC") ;; Set global leader key

  (start/leader-keys
    "SPC" '(execute-extended-command :wk "M-x")
    "." '(find-file :wk "Find file")
    "TAB" '(evil-switch-to-windows-last-buffer :wk "Last buffer")
    "/" '(+vertico/project-search :wk "Search Project")
    ;; "p" '(project-prefix-map :wk "Project command map")
    )

  (start/leader-keys
    "a" '(:ignore t :wk "Applications")
    "a r" '(ranger :wk "Ranger")
    )

  ;;(start/leader-keys
  ;;  "c" '(:ignore t :wk "Code")
  ;;  "c l" '(comment-or-uncomment-region :wk "Toggle Comments")
  ;;  )

  (start/leader-keys
    "f" '(:ignore t :wk "Find")
    "f c" '((lambda () (interactive) (find-file "~/.config/emacs/config.org")) :wk "Edit emacs config")
    "f r" '(consult-recent-file :wk "Recent files")
    "f f" '(consult-fd :wk "Fd search for files")
    "f g" '(consult-ripgrep :wk "Ripgrep search in files")
    "f l" '(consult-line :wk "Find line")
    "f i" '(consult-imenu :wk "Imenu buffer locations")
    "f s" '(save-buffer :wk "Save Buffer")
    )

  (start/leader-keys
    "b" '(:ignore t :wk "Buffer Bookmarks")
    "b b" '(consult-buffer :wk "Switch buffer")
    "b d" '(kill-current-buffer :wk "Kill buffer")
    "b i" '(ibuffer :wk "Ibuffer")
    "b n" '(next-buffer :wk "Next buffer")
    "b p" '(previous-buffer :wk "Previous buffer")
    "b r" '(revert-buffer :wk "Reload buffer")
    "b j" '(consult-bookmark :wk "Bookmark jump")
    "b s" '(scratch-buffer :wk "Scratch Buffer")
    )

  (start/leader-keys
    "d" '(:ignore t :wk "Dired")
    "j v" '(dired :wk "Open dired")
    "d j" '(dired-jump :wk "Dired jump to current")
    )

  (start/leader-keys
    "e" '(:ignore t :wk "Eglot Evaluate")
    "e e" '(eglot-reconnect :wk "Eglot Reconnect")
    "e f" '(eglot-format :wk "Eglot Format")
    "e l" '(consult-flymake :wk "Consult Flymake")
    "e b" '(eval-buffer :wk "Evaluate elisp in buffer")
    "e r" '(eval-region :wk "Evaluate elisp in region")
    )

  (start/leader-keys
    "g" '(:ignore t :wk "Git")
    "g s" '(magit-status :wk "Magit status")
    )

  (start/leader-keys
    "h" '(:ignore t :wk "Help") ;; To get more help use C-h commands (describe variable, function, etc.)
    "h q" '(save-buffers-kill-emacs :wk "Quit Emacs and Daemon")
    "h r" '((lambda () (interactive)
              (load-file "~/.config/emacs/init.el"))
        	:wk "Reload Emacs config")
    )

  (start/leader-keys
    "s" '(:ignore t :wk "Show")
    "s e" '(eat :wk "Eat terminal")
    )

  (start/leader-keys
    "t" '(:ignore t :wk "Toggle")
    "t t" '(visual-line-mode :wk "Toggle truncated lines (wrap)")
    "t l" '(display-line-numbers-mode :wk "Toggle line numbers")
    )

  (start/leader-keys
    "w" '(:ignore t :wk "Windows and Workspaces")
    "w h" '(evil-window-left :wk "Window left")
    "w l" '(evil-window-right :wk "Window right")
    "w j" '(evil-window-down :wk "Window Down")
    "w k" '(evil-window-up :wk "Window Up")
    "w /" '(evil-window-vsplit :wk "Vertical Split")
    "w -" '(evil-window-split :wk "Vertical Split")
    "w d" '(evil-window-delete :wk "Close window")
    )

  (start/leader-keys
    "p" '(:ignore t :wk "Projects")
    "p t" '(treemacs :wk "Treemacs")
    ;; Copied from project.el
    "p !" '(project-shell-command :wk "Run command")
    "p &" '(project-async-shell-command :wk "Run command (async)")
    "p f" '(project-find-file :wk "Find file")
    "p F" '(project-or-external-find-file :wk "Find file in project or external roots")
    "p b" '(project-switch-to-buffer :wk "Switch to project buffer")
    "p s" '(project-shell :wk "Run shell in project")
    "p d" '(project-find-dir :wk "Find directory")
    "p D" '(project-dired :wk "Dired")
    "p v" '(project-vc-dir :wk "Run VC-Dir")
    "p c" '(project-compile :wk "Compile Project")
    "p e" '(project-eshell :wk "Run Shell")
    "p k" '(project-kill-buffers :wk "Kill all buffers")
    "p p" '(tabspaces-open-or-create-project-and-workspace :wk "Switch Tabspaces")
    "p P" '(project-switch-project :wk "Switch Project")
    "p g" '(project-find-regexp :wk "Find matches for regexp")
    "p G" '(project-or-external-find-regexp :wk "Find matches for regexp in project or external")
    "p r" '(project-query-replace-regexp :wk "Replace regexp")
    "p x" '(project-execute-extended-command :wk "Execute extended command")
    "p o" '(project-any-command :wk "Execute any command")
    )

  (start/leader-keys
    "l" '(:ignore t :wk "Tabspaces")
    "l C" '(tabspaces-clear-buffers :wk "Clear all Buffers")
    "l b" '(tabspaces-switch-to-buffer :wk "Switch to Buffer")
    "l d" '(tabspaces-close-workspace :wk "Close Workspace")
    "l k" '(tabspaces-kill-buffers-close-workspace :wk "Kill Buffers and Close Workspace")
    "l o" '(tabspaces-open-or-create-project-and-workspace :wk "Open Project and Workspace")
    "l r" '(tabspaces-remove-current-buffer :wk "Remove current buffer")
    "l R" '(tabspaces-remove-selected-buffer :wk "Remove selected buffer")
    "l l" '(tabspaces-switch-or-create-workspace :wk "Switch or Create Workspace")
    "l t" '(tabspaces-switch-buffer-and-tab :wk "Switch Buffer and tab")
    ;; General Tab Control
    "l TAB" '(tab-previous :wk "Previous Tab")
    "l L" '(tab-move :wk "Move Tab Right")
    "l H" '((lambda () 
              (tab-move -1))
        	:wk "Move Tab Left")
    )
  )

(use-package emacs
  :custom
  (menu-bar-mode nil)         ;; Disable the menu bar
  (scroll-bar-mode nil)       ;; Disable the scroll bar
  (tool-bar-mode nil)         ;; Disable the tool bar
  ;;(inhibit-startup-screen t)  ;; Disable welcome screen

  (delete-selection-mode t)   ;; Select text and delete it by typing.
  (electric-indent-mode nil)  ;; Turn off the weird indenting that Emacs does by default.
  (electric-pair-mode f)      ;; Turns on automatic parens pairing
  (blink-cursor-mode nil)     ;; Don't blink cursor
  (global-auto-revert-mode t) ;; Automatically reload file and show changes if the file has changed

  ;;(dired-kill-when-opening-new-dired-buffer t) ;; Dired don't create new buffer
  ;;(recentf-mode t) ;; Enable recent file mode

  ;;(global-visual-line-mode t)           ;; Enable truncated lines
  ;;(display-line-numbers-type 'relative) ;; Relative line numbers
  (global-display-line-numbers-mode t)  ;; Display line numbers

  (mouse-wheel-progressive-speed nil) ;; Disable progressive speed when scrolling
  (scroll-conservatively 10) ;; Smooth scrolling
  ;;(scroll-margin 8)

  (tab-width 4)

  (make-backup-files nil) ;; Stop creating ~ backup files
  (auto-save-default nil) ;; Stop creating # auto save files
  :hook
  (prog-mode . (lambda () (hs-minor-mode t))) ;; Enable folding hide/show globally
  :config
  ;; Move customization variables to a separate file and load it, avoid filling up init.el with unnecessary variables
  (setq custom-file (locate-user-emacs-file "custom-vars.el"))
  (load custom-file 'noerror 'nomessage)
  :bind (
         ([escape] . keyboard-escape-quit) ;; Makes Escape quit prompts (Minibuffer Escape)
         )
  ;; Fix general.el leader key not working instantly in messages buffer with evil mode
  :ghook ('after-init-hook
          (lambda (&rest _)
            (when-let ((messages-buffer (get-buffer "*Messages*")))
              (with-current-buffer messages-buffer
                (evil-normalize-keymaps))))
          nil nil t)
  )

(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox-dark-medium t)) ;; We need to add t to trust this package

(add-to-list 'default-frame-alist '(alpha-background . 90)) ;; For all new frames henceforth

(set-face-attribute 'default nil
                    :font "JetBrains Mono"
                    :height 120
                    :weight 'medium)
;; This sets the default font on all graphical frames created after restarting Emacs.
;; Does the same thing as 'set-face-attribute default' above, but emacsclient fonts
;; are not right unless I also add this method of setting the default font.

(add-to-list 'default-frame-alist '(font . "JetBrains Mono")) ;; Set your favorite font
(setq-default line-spacing 0.01)

(use-package emacs
  :bind
  ("C-+" . text-scale-increase)
  ("C--" . text-scale-decrease)
  ("<C-wheel-up>" . text-scale-increase)
  ("<C-wheel-down>" . text-scale-decrease))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 25)     ;; Sets modeline height
  (doom-modeline-bar-width 5)   ;; Sets right bar width
  )

(use-package project
  :custom
  (project-switch-commands 'project-find-file)  ;; Always open find file after switching project
  )

(use-package tab-bar
  :hook (after-init . tab-bar-mode)
  )

(use-package tabspaces
  :hook (after-init . tabspaces-mode)
  :custom
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-default-tab "Default")
  (tabspaces-remove-to-default t)
  (tabspaces-include-buffers '("*scratch*"))
  (tabspaces-initialize-project-with-todo nil)
  ;; sessions
  (tabspaces-session t)
  (tabspaces-session-auto-restore t)
  (tab-bar-new-tab-choice "*scratch*")
  )

;; Filter Buffers for Consult-Buffer
(with-eval-after-load 'consult
  ;; hide full buffer list (still available with "b" prefix)
  (consult-customize consult--source-buffer :hidden t :default nil)
  ;; set consult-workspace buffer list
  (defvar consult--source-workspace
	(list :name     "Workspace Buffers"
          :narrow   ?w
          :history  'buffer-name-history
          :category 'buffer
          :state    #'consult--buffer-state
          :default  t
          :items    (lambda () (consult--buffer-query
								:predicate #'tabspaces--local-buffer-p
								:sort 'visibility
								:as #'buffer-name)))

	"Set workspace buffer list for consult-buffer.")
  (add-to-list 'consult-buffer-sources 'consult--source-workspace))

(use-package eglot
  :ensure nil ;; Don't install eglot because it's now built-in
  :hook ((python-mode ;; Autostart lsp servers for a given mode
          python-ts-mode)
         . eglot-ensure)
  :custom
  (eglot-events-buffer-size 0) ;; No event buffers (Lsp server logs)
  (eglot-autoshutdown t);; Shutdown unused servers.
  (eglot-report-progress nil) ;; Disable lsp server logs (Don't show lsp messages at the bottom, java)
  )

(defun restart-eglot ()
  (interactive)
  ;; Check if there's an active Eglot server
  (let ((current-server (eglot-current-server)))
    ;; If a server exists, prompt the user to continue
    (if current-server
          ;; Shut down the server if user confirms
          (eglot-shutdown current-server)))
  ;; Restart Eglot for the current buffer
  (eglot-ensure))

(use-package yasnippet-snippets
  :hook (prog-mode . yas-minor-mode))

(setq major-mode-remap-alist
      '((python-mode . python-ts-mode)))

(use-package python-pytest)

(use-package flymake-ruff
  :ensure t
  :hook (eglot-managed-mode . flymake-ruff-load))

(use-package micromamba
  :ensure t
  :hook (micromamba-postactivate-hook . restart-eglot)
  )

;; Add to __all__
(defsubst python-in-string/comment ()
  "Return non-nil if point is in a Python literal (a comment or string)."
  ;; We don't need to save the match data.
  (nth 8 (syntax-ppss)))

(defun python-add-to-all ()
  "Take the symbol under the point and add it to the __all__ list, if it's not already there."
  (interactive)
  (save-excursion
    (let ((thing (thing-at-point 'symbol)))
      (if (progn (goto-char (point-min))
                 (let (found)
                   (while (and (not found)
                               (re-search-forward (rx symbol-start "__all__" symbol-end
                                                      (0+ space) "=" (0+ space)
                                                      (syntax open-parenthesis))
                                                  nil t))
                     (setq found (not (python-in-string/comment))))
                   found))
          (when (not (looking-at (rx-to-string
                                  `(and (0+ (not (syntax close-parenthesis)))
                                        (syntax string-quote) ,thing (syntax string-quote)))))
            (insert (format "\'%s\', " thing)))
        (beginning-of-buffer)
        ;; Put before any import lines, or if none, the first class or
        ;; function.
        (when (re-search-forward (rx bol (or "import" "from") symbol-end) nil t)
          (re-search-forward (rx symbol-start (or "def" "class") symbol-end) nil t))
        (forward-line -1)
        (insert (format "\n__all__ = [\'%s\']\n\n" thing))))))

(defun +python-executable-find (exe)
  "Resolve the path to the EXE executable.
    Tries to be aware of your active conda/pipenv/virtualenv environment, before
    falling back on searching your PATH."
  (if (file-name-absolute-p exe)
      (and (file-executable-p exe)
           exe)
    (let ((exe-root (format "bin/%s" exe)))
      (cond ((when python-shell-virtualenv-root
               (let ((bin (expand-file-name exe-root python-shell-virtualenv-root)))
                 (if (file-exists-p bin) bin))))
            ((when (require 'conda nil t)
               (let ((bin (expand-file-name (concat conda-env-current-name "/" exe-root)
                                            (conda-env-default-location))))
                 (if (file-executable-p bin) bin))))
            ((when-let (bin (projectile-locate-dominating-file default-directory exe-root))
               (setq-local doom-modeline-python-executable (expand-file-name exe-root bin))))
            ((executable-find exe))))))

(defun +python/open-repl ()
  "Open the Python REPL."
  (interactive)
  (require 'python)
  (unless python-shell-interpreter
    (user-error "`python-shell-interpreter' isn't set"))
  (pop-to-buffer
   (process-buffer
    (let ((dedicated (bound-and-true-p python-shell-dedicated)))
      (if-let* ((pipenv (+python-executable-find "pipenv"))
                (pipenv-project (pipenv-project-p)))
          (let ((default-directory pipenv-project)
                (python-shell-interpreter-args
                 (format "run %s %s"
                         python-shell-interpreter
                         python-shell-interpreter-args))
                (python-shell-interpreter pipenv))
            (run-python nil dedicated t))
        (run-python nil dedicated t))))))

(defun +python/open-ipython-repl ()
  "Open an IPython REPL."
  (interactive)
  (require 'python)
  (let ((python-shell-interpreter
         (or (+python-executable-find (car +python-ipython-command))
             "ipython"))
        (python-shell-interpreter-args
         (string-join (cdr +python-ipython-command) " ")))
    (+python/open-repl)))

(defvar +python-ipython-command '("ipython" "-i" "--simple-prompt" "--no-color-info")
  "Command to initialize the ipython REPL for `+python/open-ipython-repl'.")

(defun cadair/run-restart-repl ()
  "Run a new python repl in a window which does not have focus."
  (interactive)
  (setq initial-buffer (current-buffer))
  (if (python-shell-get-buffer)
      (kill-buffer (python-shell-get-buffer)))
  (+python/open-ipython-repl)
  (evil-normal-state)
  (pop-to-buffer initial-buffer)
  )

(defun cadair/run-in-repl (arg)
  "Run a python buffer in a new ipython repl"
  (interactive "P")
  (cadair/run-restart-repl)
  (run-at-time 0.5 nil 'python-shell-send-buffer)
  )

(defun cadair/run-in-repl-switch (arg)
  "Run a python buffer in a new ipython repl"
  (interactive "P")
  (cadair/run-restart-repl)
  (run-at-time 0.5 nil 'python-shell-send-buffer)
  (run-at-time 1.0 nil (pop-to-buffer (python-shell-get-buffer)))
  )

(defun cadair/python-execute-file (arg)
  "Execute a python script in a shell."
  (interactive "P")
  ;; set compile command to buffer-file-name
  ;; universal argument put compile buffer in comint mode
  (let ((universal-argument t)
        (compile-command (format "python %s"
                                 (shell-quote-argument (file-name-nondirectory buffer-file-name)))))
    (if arg
        (call-interactively 'compile)
      (compile compile-command t)
      (with-current-buffer (get-buffer "*compilation*")
        (inferior-python-mode)))))

(my-local-leader
  :states 'normal
  :keymaps 'python-ts-mode-map
  "t a" 'python-pytest
  "t f" 'python-pytest-file-dwim
  "t F" 'python-pytest-file
  "t t" 'python-pytest-run-def-or-class-at-point-dwim
  "t T" 'python-pytest-run-def-or-class-at-point
  "t r" 'python-pytest-repeat
  "t p" 'python-pytest-dispatch

  "c" 'cadair/python-execute-file
  "r" 'cadair/run-in-repl
  "R" 'cadair/run-in-repl-switch
  "a" 'python-add-to-all
  "g g" 'evil-goto-definition
  )

(use-package org
  :ensure nil
  :custom
  (org-edit-src-content-indentation 4) ;; Set src block automatic indent to 4 instead of 2.

  :hook
  (org-mode . org-indent-mode) ;; Indent text
  ;; The following prevents <> from auto-pairing when electric-pair-mode is on.
  ;; Otherwise, org-tempo is broken when you try to <s TAB...
  ;;(org-mode . (lambda ()
  ;;              (setq-local electric-pair-inhibit-predicate
  ;;                          `(lambda (c)
  ;;                             (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))
  )

(use-package toc-org
  :commands toc-org-enable
  :hook (org-mode . toc-org-mode))

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode))

(use-package org-tempo
  :ensure nil
  :after org)

(use-package eat
  :hook ('eshell-load-hook #'eat-eshell-mode))

;; (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; (require 'start-multiFileExample)

;; (start/hello)

(use-package nerd-icons
  :if (display-graphic-p))

(use-package nerd-icons-dired
  :hook (dired-mode . (lambda () (nerd-icons-dired-mode t))))

(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package magit
  :commands magit-status)

(use-package diff-hl
  :hook ((dired-mode         . diff-hl-dired-mode-unless-remote)
         (magit-pre-refresh  . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :init (global-diff-hl-mode))

(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-prefix 2)          ;; Minimum length of prefix for auto completion.
  (corfu-popupinfo-mode t)       ;; Enable popup information
  (corfu-popupinfo-delay 0.5)    ;; Lower popupinfo delay to 0.5 seconds from 2 seconds
  (corfu-separator ?\s)          ;; Orderless field separator, Use M-SPC to enter separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin
  (completion-ignore-case t)
  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)
  (corfu-preview-current nil) ;; Don't insert completion without confirmation
  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode))

(use-package nerd-icons-corfu
  :after corfu
  :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package cape
  :after corfu
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  ;; The functions that are added later will be the first in the list

  (add-to-list 'completion-at-point-functions #'cape-dabbrev) ;; Complete word from current buffers
  (add-to-list 'completion-at-point-functions #'cape-dict) ;; Dictionary completion
  (add-to-list 'completion-at-point-functions #'cape-file) ;; Path completion
  (add-to-list 'completion-at-point-functions #'cape-elisp-block) ;; Complete elisp in Org or Markdown mode
  (add-to-list 'completion-at-point-functions #'cape-keyword) ;; Keyword/Snipet completion

  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev) ;; Complete abbreviation
  ;;(add-to-list 'completion-at-point-functions #'cape-history) ;; Complete from Eshell, Comint or minibuffer history
  ;;(add-to-list 'completion-at-point-functions #'cape-line) ;; Complete entire line from current buffer
  ;;(add-to-list 'completion-at-point-functions #'cape-elisp-symbol) ;; Complete Elisp symbol
  ;;(add-to-list 'completion-at-point-functions #'cape-tex) ;; Complete Unicode char from TeX command, e.g. \hbar
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml) ;; Complete Unicode char from SGML entity, e.g., &alpha
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345) ;; Complete Unicode char using RFC 1345 mnemonics
  )

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package vertico
  :init
  (vertico-mode))

(savehist-mode) ;; Enables save history mode

(use-package marginalia
  :after vertico
  :init
  (marginalia-mode))

(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  :hook
  ('marginalia-mode-hook . 'nerd-icons-completion-marginalia-setup))

(cl-defun +vertico-file-search (&key query in all-files (recursive t) prompt args)
  "Conduct a file search using ripgrep.

:query STRING
  Determines the initial input to search for.
:in PATH
  Sets what directory to base the search out of. Defaults to the current project's root.
:recursive BOOL
  Whether or not to search files recursively from the base directory.
:args LIST
  Arguments to be appended to `consult-ripgrep-args'."
  (declare (indent defun))
  (unless (executable-find "rg")
    (user-error "Couldn't find ripgrep in your PATH"))
  (require 'consult)
  (setq deactivate-mark t)
  (let* ((project-root (or default-directory))
         (directory (or in project-root))
         (consult-ripgrep-args
          (concat "rg "
                  (if all-files "-uu ")
                  (unless recursive "--maxdepth 1 ")
                  "--null --line-buffered --color=never --max-columns=1000 "
                  "--path-separator /   --smart-case --no-heading "
                  "--with-filename --line-number --search-zip "
                  "--hidden -g !.git -g !.svn -g !.hg "
                  (mapconcat #'identity args " ")))
         (prompt (if (stringp prompt) (string-trim prompt) "Search"))
         (query)
         (consult-async-split-style consult-async-split-style)
         (consult-async-split-styles-alist consult-async-split-styles-alist))
    ;; Change the split style if the initial query contains the separator.
    (when query
      (cl-destructuring-bind (&key type separator initial _function)
          (consult--async-split-style)
        (pcase type
          (`separator
           (replace-regexp-in-string (regexp-quote (char-to-string separator))
                                     (concat "\\" (char-to-string separator))
                                     query t t))
          (`perl
           (when (string-match-p initial query)
             (setf (alist-get 'perlalt consult-async-split-styles-alist)
                   `(:initial ,(or (cl-loop for char in (list "%" "@" "!" "&" "/" ";")
                                            unless (string-match-p char query)
                                            return char)
                                   "%")
                     :type perl)
                   consult-async-split-style 'perlalt))))))
    (consult--grep prompt #'consult--ripgrep-make-builder directory query)))

(defun +vertico/project-search (&optional arg initial-query directory)
  "Performs a live project search from the project root using ripgrep.
If ARG (universal argument), include all files, even hidden or compressed ones,
in the search."
  (interactive "P")
  (+vertico-file-search :query initial-query :in directory :all-files arg))

(use-package consult
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))

  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  ;; (consult-customize
  ;; consult-theme :preview-key '(:debounce 0.2 any)
  ;; consult-ripgrep consult-git-grep consult-grep
  ;; consult-bookmark consult-recent-file consult-xref
  ;; consult--source-bookmark consult--source-file-register
  ;; consult--source-recent-file consult--source-project-recent-file
  ;; :preview-key "M-."
  ;; :preview-key '(:debounce 0.4 any))

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
   ;;;; 1. project.el (the default)
   ;; (setq consult-project-function #'consult--default-project--function)
   ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
   ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
   ;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
   ;;;; 5. No project support
  ;; (setq consult-project-function nil)
  )

(use-package diminish)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init
  (which-key-mode 1)
  :diminish
  :custom
  (which-key-side-window-location 'bottom)
  (which-key-sort-order #'which-key-key-order-alpha) ;; Same as default, except single characters are sorted alphabetically
  (which-key-sort-uppercase-first nil)
  (which-key-add-column-padding 1) ;; Number of spaces to add to the left of each column
  (which-key-min-display-lines 6)  ;; Increase the minimum lines to display, because the default is only 1
  (which-key-idle-delay 0.8)       ;; Set the time delay (in seconds) for the which-key popup to appear
  (which-key-max-description-length 25)
  (which-key-allow-imprecise-window-fit nil)) ;; Fixes which-key window slipping out in Emacs Daemon

(use-package treemacs
  :ensure t
  :defer t
  :init (treemacs-project-follow-mode)
  )
(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t
  )
(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t
  )

(use-package ranger)

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
;; Increase the amount of data which Emacs reads from the process
(setq read-process-output-max (* 1024 1024)) ;; 1mb
