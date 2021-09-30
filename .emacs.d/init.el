;;; GNU/Linux

(when (eq system-type 'gnu/linux)
  ;; Add GNU/Linux specific stuff here
  )

;;; macOS
(when (eq system-type 'darwin)
  ;; Ensure environment  variables inside Emacs look  the same as in  the user's
  ;; shell.
    (toggle-frame-maximized)
    (setq projectile-enable-caching t))
;;; Windows
(when (eq system-type 'windows-nt)
  ;; Add Windows specific stuff here
    ;; Kudos to Jeffrey Snover: https://docs.microsoft.com/en-us/archive/blogs/dotnetinterop/run-powershell-as-a-shell-within-emacs
    (setq explicit-shell-file-name "powershell.exe")
    (setq explicit-powershell.exe-args '()))

(require 'server)
(when (not (server-running-p))
    (server-start))

(defun mati/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                   (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'mati/display-startup-time)

;; Put backup files neatly away                                                 
(let ((backup-dir "~/.emacs.d/backups")
      (auto-saves-dir "~/.emacs.d/auto-saves/"))
  (dolist (dir (list backup-dir auto-saves-dir))
    (when (not (file-directory-p dir))
      (make-directory dir t)))
  (setq backup-directory-alist `(("." . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,auto-saves-dir t))
        auto-save-list-file-prefix (concat auto-saves-dir ".saves-")
        tramp-backup-directory-alist `((".*" . ,backup-dir))
        tramp-auto-save-directory auto-saves-dir))

(setq backup-by-copying t    ; Don't delink hardlinks                           
      delete-old-versions t  ; Clean up the backups                             
      version-control t      ; Use version numbers on backups,                  
      kept-new-versions 5    ; keep some new versions                           
      kept-old-versions 2)   ; and some old ones, too

;; Initialize package
(require 'package)
  
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			("melpa-stable" . "https://stable.melpa.org/packages/")
			("org" . "https://orgmode.org/elpa/")
			("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
(package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (or (package-installed-p 'use-package))
    (package-install 'use-package))
(require 'use-package)

;; Uncomment this to get a reading on packages that get loaded at startup
;; (setq use-package-verbose t)

(setq use-package-always-ensure t)

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
    (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
    (bootstrap-version 5))
(unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	    "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	    'silent 'inhibit-cookies)
    (goto-char (point-max))
    (eval-print-last-sexp)))
(load bootstrap-file nil 'nomessage))


;; Always use straight to install on systems other than Linux
(setq straight-use-package-by-default (not (eq system-type 'gnu/linux)))

;; Use straight.el for use-package expressions
(straight-use-package 'use-package)

;; Load the helper package for commands like `straight-x-clean-unused-repos'
(require 'straight-x)

(use-package auto-package-update
    :custom
    (auto-package-update-interval 7)
    (auto-package-update-prompt-before-update t)
    (auto-package-update-hide-results t)
    :config
    (auto-package-update-maybe)
    (auto-package-update-at-time "20:00"))

;; Thanks, but no thanks
(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)          ; Disable the menu bar

(setq visible-bell t)

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)

;; Enable line numbers for some modes
(dolist (mode '(text-mode-hook
		prog-mode-hook
		conf-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 1))))

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                shell-mode-hook
		treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(fset 'yes-or-no-p 'y-or-n-p)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (setq exec-path-from-shell-variables '("PATH" "GOPATH"))
  (exec-path-from-shell-initialize))

(pcase system-type
    ((or 'windows-nt 'cygwin)
	(set-face-attribute 'default nil
			:font "JetBrains Mono"
			:weight 'light
			:height 100)))

(use-package doom-themes
    :defer t)

(load-theme 'doom-palenight t)
(doom-themes-visual-bell-config)

(use-package rainbow-delimiters
    :hook (emacs-lisp-mode . rainbow-delimiters-mode))

;; Shotcut suggestions
(use-package which-key
    :init (which-key-mode)
    :diminish which-key-mode
    :config
	(setq which-key-idle-delay 0.3))

;; Ivy
(use-package swiper :ensure t)

(use-package ivy
    :diminish
    :bind (:map ivy-minibuffer-map
	    ("TAB" . ivy-alt-done)
	    ("C-f" . ivy-alt-done)
	    ("C-l" . ivy-alt-done)
	    ("C-j" . ivy-next-line)
	    ("C-k" . ivy-previous-line)
	   :map ivy-switch-buffer-map
	    ("C-k" . ivy-previous-line)
	    ("C-l" . ivy-done)
	    ("C-d" . ivy-switch-buffer-kill)
	:map ivy-reverse-i-search-map
	    ("C-k" . ivy-previous-line)
	    ("C-d" . ivy-reverse-i-search-kill))
    :init
	(ivy-mode 1))

(use-package counsel
    :demand t
    :bind (("M-x" . counsel-M-x)
	("C-x b" . counsel-ibuffer)
	("C-x C-f" . counsel-find-file)
	;; ("C-M-j" . counsel-switch-buffer)
	("C-M-l" . counsel-imenu)
    :map minibuffer-local-map
	("C-r" . 'counsel-minibuffer-history))
    :custom
	(counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
    :config
	(setq ivy-initial-inputs-alist nil))


(use-package ivy-rich
    :init
	(ivy-rich-mode 1))

(use-package diminish
    :ensure t)

(use-package doom-modeline
    :init (doom-modeline-mode 1)) ; run M-x all-the-icons-install-fonts

(use-package helpful
    :custom
	(counsel-describe-function-function #'helpful-callable)
	(counsel-describe-variable-function #'helpful-variable)
    :bind
	([remap describe-function] . counsel-describe-function)
	([remap describe-symbol] . helpful-symbol)
	([remap describe-variable] . counsel-describe-variable)
	([remap describe-command] . helpful-command)
	([remap describe-key] . helpful-key))

(use-package treemacs)

(use-package treemacs-evil
  :after evil)

(use-package treemacs-projectile
  :after projectile)

(use-package general
    :config
	(general-evil-setup t)
	(general-create-definer mati/leader-keys
	    :keymaps '(normal insert visual emacs)
	    :prefix "SPC"
	    :global-prefix"C-SPC"))

(custom-set-variables
    ;; custom-set-variables was added by Custom.
    ;; If you edit it by hand, you could mess it up, so be careful.
    ;; Your init file should contain only one such instance.
    ;; If there is more than one, they won't work right.
    '(mac-command-modifier 'control)
    '(mac-control-modifier nil)
    '(mac-option-modifier 'meta)
    '(mac-right-control-modifier 'left)
    '(mac-right-option-modifier 'alt))
(custom-set-faces
    ;; custom-set-faces was added by Custom.
    ;; If you edit it by hand, you could mess it up, so be careful.
    ;; Your init file should contain only one such instance.
    ;; If there is more than one, they won't work right.
)

(defun mati/evil-hook ()
    (dolist (mode '(custom-mode
	    eshell-mode
	    git-rebase-mode
	    erc-mode
	    circe-server-mode
	    circe-chat-mode
	    circe-query-mode
	    sauron-mode
	    term-mode))
	(add-to-list 'evil-emacs-state-modes mode)))

(use-package undo-tree			
    :init
	(global-undo-tree-mode 1)
    :bind (:map undo-tree-map
		("C-/" . nil)
		("C-z" . 'undo-tree-undo)
		("C-S-z" . 'undo-tree-redo)))

(use-package evil
    :init
	(setq evil-want-integration t)
	(setq evil-want-keybinding nil)
	(setq evil-want-C-u-scroll t)
	(setq evil-want-C-i-jump nil)
	(setq evil-respect-visual-line-mode t)
	(setq evil-undo-system 'undo-tree)
    :bind (:map evil-insert-state-map
	    ("C-g" . 'evil-normal-state)
	    ("C-h" . 'evil-delete-backward-char-and-join)
	    ("C-z" . nil)
       :map evil-motion-state-map
            ("C-z" . nil))
    :config
	(add-hook 'evil-mode-hook 'mati/evil-hook)
	(evil-mode 1)
	
	(define-key evil-normal-state-map (kbd "SPC h") (general-simulate-key "C-h"))
	(define-key evil-normal-state-map (kbd "SPC x") (general-simulate-key "C-x"))
	(define-key evil-normal-state-map (kbd "SPC c") (general-simulate-key "C-c"))
	(define-key evil-normal-state-map (kbd "SPC m") (general-simulate-key "M-x"))
	(define-key evil-normal-state-map (kbd "SPC w") (general-simulate-key "C-w"))
	
	(define-key evil-insert-state-map (kbd "C-SPC h") (general-simulate-key "C-h"))
	(define-key evil-insert-state-map (kbd "C-SPC x") (general-simulate-key "C-x"))
	(define-key evil-insert-state-map (kbd "C-SPC c") (general-simulate-key "C-c"))
	(define-key evil-insert-state-map (kbd "C-SPC m") (general-simulate-key "M-x"))
	(define-key evil-insert-state-map (kbd "C-SPC w") (general-simulate-key "C-W"))
	
	;; Use visual line motions even outside of visual-line-mode buffers
	(evil-global-set-key 'motion "j" 'evil-next-visual-line)
	(evil-global-set-key 'motion "k" 'evil-previous-visual-line)


	(evil-ex-define-cmd "q" 'kill-this-buffer)
	(evil-ex-define-cmd "qa" 'evil-quit)

	(evil-set-initial-state 'messages-buffer-mode 'normal)
	(evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
    :after evil
    :config (evil-collection-init))

(use-package evil-org
    :ensure t
    :after org
    :hook (org-mode . (lambda () evil-org-mode))
    :config
    (require 'evil-org-agenda)
    (evil-org-agenda-set-keys))

(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
    "scale text"
    ("j" text-scale-increase "in")
    ("k" text-scale-decrease "out")
    ("0" (text-scale-adjust 0) "normal") 
    ("f" nil "finished" :exit t))

  (defun mati/open-config ()
      (interactive)
      (find-file (expand-file-name "~/.emacs.d/Emacs.org")))

  (mati/leader-keys
      "q" '(org-capture :which-key "capture")
      "a" '(:ingore true :which-key "agenda")
      "aa" '(org-agenda-list :which-key "agenda")
      "ad" '(org-agenda :which-key "dashboard")
      "df" '(org-roam-node-find :which-key "find node")
      "di" '(org-roam-node-insert :which-key "insert node")
      "dt" '(org-roam-dailies-goto-today :which-key "today")

      "b" '(:ignore t :which-key "buffer")
      "bi" '(ibuffer :which-key "ibuffer")
      "bk" '(kill-this-buffer :which-key "kill buffer")
      "bo" '(counsel-ibuffer :which-key "open") 

      "t" '(:ignore t :which-key "toggles")
      "tt" '(treemacs :which-key "browse dir tree")
      "ts" '(hydra-text-scale/body :which-key "scale text")
      "tr" '(org-roam-buffer-toggle :which-key "roam buffer")

      "p" '(:ignore t :which-key "project") 
      "pf"  'projectile-find-file
      "po"  '(projectile-switch-project :which-key "open")
      "pF"  'consult-ripgrep
      "pc"  'projectile-compile-project
      "pd"  'projectile-dired 
      "pn"  '(projectile-add-known-project :which-key "new project") 

      "g"   '(:ignore t :which-key "git")
      "gs"  'magit-status
      "gd"  'magit-diff-unstaged
      "gc"  'magit-branch-or-checkout
      "gl"   '(:ignore t :which-key "log")
      "glc" 'magit-log-current
      "glf" 'magit-log-buffer-file
      "gb"  'magit-branch
      "gP"  'magit-push-current
      "gp"  'magit-pull-branch
      "gf"  'magit-fetch
      "gF"  'magit-fetch-all
      "gr"  'magit-rebase

      "u" '(:ignore true :which-key "utilities")

      "f" '(:ignore true :which-key "files")
      "fo" '(find-file :which-key "open")
      "." '(find-file :which-key "open file")
      "fd" '(dired :which-key "directory")
      "fc" '(mati/open-config :which-key "config")

      ":" '(eval-expression :which-key "eval")
      "s" '(shell :which-key "shell")
      "/" 'swiper
      "h" '(:ignore true :which-key "help")
      "d" '(:ignore true :which-key "docs")
      "c" '(:ignore true :which-key "C-c")
      "w" '(:ignore true :which-key "window")
      "m" '(:ignore true :which-key "M-x")
      "x" '(:ignore true :which-key "C-x"))

  ;; (general-define-key
  ;;  :keymaps '(org-mode-map)
  ;;  :prefix "SPC"
  ;;  :global-prefix "C-SPC"
  ;;  "dd" '(org-roam-dailies-map))

  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)

  (global-unset-key (kbd "C-x C-b"))
  (global-unset-key (kbd "C-x C-@"))
  (global-unset-key (kbd "C-x C-<SPC>"))
  (global-unset-key (kbd "C-/"))

(use-package projectile
    :diminish projectile-mode
    :config (projectile-mode)
    :demand t
    :bind ("C-M-p" . projectile-find-file)
    :bind-keymap
    ("C-c p" . projectile-command-map)
    :init
    (when (file-directory-p "~/Projects/")
    (setq projectile-project-search-path '("~/Projects/")))
    (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
    :after projectile
    :config
    (counsel-projectile-mode))

(use-package magit)

(defun mati/lsp-mode-setup ()
(define-key evil-normal-state-map (kbd "SPC l") (general-simulate-key "C-l"))
(define-key evil-insert-state-map (kbd "C-SPC l") (general-simulate-key "C-l"))
(setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
(lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
:commands (lsp lsp-deferred)
:hook ((lsp-mode . mati/lsp-mode-setup)
    (c-mode . lsp-deferred)
    (c++-mode . lsp-deferred)
    (go-mode . lsp-deferred)
    (prog-mode . lsp-mode))
:init
(setq lsp-keymap-prefix "C-l")  ;; Or 'C-l', 's-l'
:config
(lsp-enable-which-key-integration t)
(setq lsp-clients-clangd-args '("--header-insertion=never"
				"--completion-style=bundled"
				"--background-index")))

(use-package lsp-ui
:hook (lsp-mode . lsp-ui-mode)
:custom
(lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
:after lsp
:commands (lsp-treemacs-errors-list
	lsp-treemacs-symbols)
:config
    (lsp-treemacs-sync-mode t))

(use-package lsp-ivy)

(add-hook 'prog-mode-hook #'hs-minor-mode)

(use-package company
:after lsp-mode
:hook (lsp-mode . company-mode)
:bind (:map company-active-map ("C-TAB" . company-complete-selection))
    (:map lsp-mode-map ("C-TAB" . company-indent-or-complete-common))
:custom
(company-minimum-prefix-length 1)
(company-idle-delay 0.0))

(use-package company-box
:hook (company-mode . company-box-mode))

(use-package flycheck
  :init   (global-flycheck-mode t))

(use-package yasnippet
  :config
  (setq yas-indent-line 'fixed)
  (yas-reload-all)
  :hook   (prog-mode . yas-minor-mode))

(use-package evil-nerd-commenter
:bind ("C-/" . evilnc-comment-or-uncomment-lines))

(use-package xterm-color
  :straight (xterm-coloer :type git :host github :repo "atomontage/xterm-color"))


(setq comint-output-filter-functions
      (remove 'ansi-color-process-output comint-output-filter-functions))

(add-hook 'shell-mode-hook
          (lambda ()
            ;; Disable font-locking in this buffer to improve performance
            (font-lock-mode -1)
            ;; Prevent font-locking from being re-enabled in this buffer
            (make-local-variable 'font-lock-function)
            (setq font-lock-function (lambda (_) nil))
            (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)))

(use-package dap-mode
  ;; Uncomment the config below if you want all UI panes to be hidden by default!
  ;; :custom
  ;; (lsp-enable-dap-auto-configure nil)
  ;; :config
  ;; (dap-ui-mode 1)

  :config
  ;; install native debug. The first time run dap-gdb-lldb-setup
  ;; Then do dap-debug or dap-debug-edit-template and selet GBD or LLDB configuration.
  (require 'dap-gdb-lldb)

  ;; install go debug. The first time run dap-go-setup
  (require 'dap-go)

  ;; install python debug. Run pip install "ptvsd>=4.2"
  (require 'dap-python)

  ;; powershell debug.
  (require 'dap-pwsh)
  
  ;; Bind `C-c l d` to `dap-hydra` for easy access
  (general-define-key
    :keymaps 'lsp-mode-map
    :prefix lsp-keymap-prefix
    "d" '(dap-hydra t :wk "debugger")))

(use-package modern-cpp-font-lock
:config (modern-c++-font-lock-global-mode t))

(use-package clang-format)

(use-package highlight-doxygen)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-hook
 'c++-mode-hook
 (lambda ()
   (setq c-default-style "bsd"
	 c-basic-offset  4
	 tab-width       4)
   (c-set-offset 'innamespace 0)
   (highlight-doxygen-mode t)
   ;; https://www.emacswiki.org/emacs/ElectricPair
   (electric-pair-mode t)
   ;; https://wikemacs.org/wiki/Subword-mode
   (subword-mode t)
   (display-line-numbers-mode t)
   ;; Set the right margin according to Epic Games coding standard
   (setq-local fill-column 120)

   (evil-local-set-key 'normal (kbd "SPC u u") 'ue-command-map)
   (evil-local-set-key 'normal (kbd "SPC u f") 'clang-format-buffer)
   (evil-local-set-key 'normal (kbd "SPC u y") 'company-yasnippet)))

  ;; (use-package ue
    ;; :init   (ue-global-mode t))
(use-package ue
  :straight (ue :type git :host gitlab :repo "unrealemacs/ue.el")
  :init (ue-global-mode))

;;Goimports
(defun go-mode-setup ()
  ;(go-eldoc-setup)
  (add-hook 'before-save-hook 'gofmt-before-save) 
  (setq gofmt-command "goimports"))

(use-package go-mode
  :hook (go-mode . go-mode-setup))

(use-package org
:bind (:map org-mode-map
	    ("C-c c" . org-ctrl-c-ctrl-c)
	    ("C-c t" . org-todo)
	    ("C-c d" . org-deadline)
	    ("C-c s" . org-schedule)
	    ("C-c w" . org-refile)
	    ("C-c a" . org-attach))
:config
    (setq org-agenda-files '("~/org/todo.org"
			    "~/org/contacts.org"))
    (setq org-agenda-start-with-log-mode t)
    (setq org-log-done 'time)
    (setq org-log-into-drawer t)

    (require 'org-habit)
    (add-to-list 'org-modules 'org-habit)
    (setq org-habit-graph-column 60)

    (setq org-capture-templates
      `(("t" "Task" entry (file+olp "~/org/todo.org" "Inbox")
		"* TODO %?\n  SCHEDULED: %U\n  %a\n  %i" :empty-lines 1)

	("j" "Journal" entry
		(file+olp+datetree "~/org/journal.org")
		"\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
		;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
		:clock-in :clock-resume
		:empty-lines 1)
	("n" "notes" entry
		(file+olp+datetree "~/org/notes.org")
		"* %<%I:%M %p> - %a :notes:\n\n%?\n\n"
		:clock-in :clock-resume
		:empty-lines 1)
	("m" "Metrics Capture")
	("mw" "Weight" table-line
	    (file+headline "~/org/gym.org" "Weight")
	    "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)))

    (setq org-refile-targets
	'(("notes.org" :maxlevel . 1)
	("todo.org" :maxlevel . 1)))


    ;; Configure autosaving
    (advice-add 'org-agenda-todo :after
		(lambda (&rest _)
		    (org-save-all-org-buffers)))

    (setq org-support-shift-select t)
    (setq org-todo-keywords
	    '((sequence "TODO(t)" "SOMEDAY(s)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
    ;; Configure custom agenda views
    (setq org-agenda-custom-commands
	'(("W" "Work Tasks" tags "+Au"))))


(use-package org-bullets
:hook (org-mode . org-bullets-mode)
:custom (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(org-babel-do-load-languages
'org-babel-load-languages
'((emacs-lisp . t)
(python . t)
(C . t)
(org . t)))
(setq org-confirm-babel-evaluate nil)  
(setq org-src-preserve-indentation t)

(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("cpp" . "src c++"))

;; Automatically tangle our Emacs.org config file when we save it
(defun mati/org-babel-tangle-config ()
(when (string-equal (buffer-file-name)
		(expand-file-name "~/.emacs.d/Emacs.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
    (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'mati/org-babel-tangle-config)))

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/org")
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("b" "book notes" plain
     "\n* Source\n\nAuthor: %^{Author}\nTitle: ${title}\nYear: %^{Year}\n\n* Summary\n\n%?"
     :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
     :unnarrowed t)
     ("p" "project" plain "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
	:if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: Project")
	:unnarrowed t)))
  (org-roam-dailies-capture-templates
    '(("d" "default" entry "* %<%I:%M %p>: %?"
       :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
  :bind (:map org-mode-map
        ("C-TAB"  . completion-at-point)
	:map org-roam-dailies-map
	("Y" . org-roam-dailies-capture-yesterday)
        ("T" . org-roam-dailies-capture-tomorrow))
  :config
    (require 'org-roam-dailies) ;; Ensure the keymap is available
    (defun mati/org-roam-copy-todo-to-today ()
    (interactive)
    (let ((org-refile-keep t) ;; Set this to nil to delete the original!
	    (org-roam-dailies-capture-templates
	    '(("t" "tasks" entry "%?"
		:if-new (file+head+olp "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n" ("Tasks")))))
	    (org-after-refile-insert-hook #'save-buffer)
	    today-file
	    pos)
	(save-window-excursion
	(org-roam-dailies--capture (current-time) t)
	(setq today-file (buffer-file-name))
	(setq pos (point)))

	;; Only refile if the target file is different than the current file
	(unless (equal (file-truename today-file)
		    (file-truename (buffer-file-name)))
	(org-refile nil nil (list "Tasks" today-file nil pos)))))

    (add-to-list 'org-after-todo-state-change-hook
		(lambda ()
		(when (equal org-state "DONE")
		    (mati/org-roam-copy-todo-to-today))))
    (org-roam-db-autosync-mode)
    (org-roam-setup))

(setq erc-server "irc.libera.chat"
      erc-nick "enki97"    ; Change this!
      erc-user-full-name "Mati A"  ; And this!
      erc-track-shorten-start 8
      erc-autojoin-channels-alist '(("irc.libera.chat" "#systemcrafters" "#emacs"))
	erc-kill-buffer-on-part t
	erc-auto-query 'bury)

(use-package telega)

(use-package elcord
  :config (elcord-mode))
