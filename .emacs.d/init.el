(require 'server)
(when (not (server-running-p))
    (server-start))

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

;; Override some modes which derive from the above
(dolist (mode '(org-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0))))

(pcase system-type
    ((or 'gnu/linux 'windows-nt 'cygwin)
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
    :bind (("C-s" . swiper)
	:map ivy-minibuffer-map
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

(use-package general
    :config
	(general-evil-setup t)
	(general-create-definer mati/leader-keys
	    :keymaps '(normal insert visual emacs)
	    :prefix "SPC"
	    :global-prefix "C-SPC"))

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
:config
    (add-hook 'evil-mode-hook 'mati/evil-hook)
    (evil-mode 1)

    (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
    (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

    (define-key evil-normal-state-map (kbd "SPC h") (general-simulate-key "C-h"))
    (define-key evil-normal-state-map (kbd "SPC x") (general-simulate-key "C-x"))
    (define-key evil-normal-state-map (kbd "<SPC>c") (general-simulate-key "C-c"))

    ;; Use visual line motions even outside of visual-line-mode buffers
    (evil-global-set-key 'motion "j" 'evil-next-visual-line)
    (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

    (evil-set-initial-state 'messages-buffer-mode 'normal)
    (evil-set-initial-state 'dashboard-mode 'normal))


(use-package evil-collection
:after evil
:config
(evil-collection-init))

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

    (mati/leader-keys
	"q" '(org-capture :which-key "capture")
	"a" '(:ingore true :which-key "agenda")
	"aa" '(org-agenda-list :which-key "agenda")
	"ad" '(org-agenda :which-key "dashboard")

	"b" '(:ignore t :which-key "buffer")
	"bi" '(ibuffer :which-key "ibuffer")
	"bk" '(kill-this-buffer :which-key "kill buffer")
	"bo" '(counsel-ibuffer :which-key "open") 

	"t" '(:ignore t :which-key "toggles")
	"tt" '(counsel-load-theme :which-key "choose theme")
	"ts" '(hydra-text-scale/body :which-key "scale text")

	"p" '(:ignore t :which-key "project") 
	"pf"  'projectile-find-file
	"ps"  'projectile-switch-project
	"pF"  'consult-ripgrep
	"pp"  'projectile-find-file
	"pc"  'projectile-compile-project
	"pd"  'projectile-dired 

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

	"f" '(:ignore true :which-key "files")
	"fo" '(find-file :which-key "open")
	"h" '(:ignore true :which-key "help")
	"c" '(:ignore true :which-key "C-c")
	"x" '(:ignore true :which-key "C-x"))


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
	(setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
	(lsp-headerline-breadcrumb-mode))

    (use-package lsp-mode
	:commands (lsp lsp-deferred)
	:hook (lsp-mode . mati/lsp-mode-setup)
	:init
	(setq lsp-keymap-prefix "SPC l")  ;; Or 'C-l', 's-l'
	:config
	(lsp-enable-which-key-integration t))

    (use-package lsp-ui
	:hook (lsp-mode . lsp-ui-mode)
	:custom
	(lsp-ui-doc-position 'bottom))

    (use-package lsp-treemacs
	:after lsp)

    (use-package lsp-ivy)

    (use-package company
	:after lsp-mode
	:hook (lsp-mode . company-mode)
	:bind (:map company-active-map ("<tab>" . company-complete-selection))
	    (:map lsp-mode-map ("<tab>" . company-indent-or-complete-common))
	:custom
	(company-minimum-prefix-length 1)
	(company-idle-delay 0.0))

    (use-package company-box
	:hook (company-mode . company-box-mode))

(use-package evil-nerd-commenter
  :bind ("C-/" . evilnc-comment-or-uncomment-lines))

    (use-package ue
	:straight (ue :type git :host gitlab :repo "unrealemacs/ue.el")
	:init (ue-global-mode))

      (use-package org
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
	  `(("t" "Tasks / Projects")
	  ("tt" "Task" entry (file+olp "~/org/todo.org" "Inbox")
		  "* TODO %?\n  SCHEDULED: %U\n  %a\n  %i" :empty-lines 1)
      
	  ("j" "Journal Entries")
	  ("jj" "Journal" entry
		  (file+olp+datetree "~/org/journal.org")
		  "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
		  ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
		  :clock-in :clock-resume
		  :empty-lines 1)
	  ("jm" "notes" entry
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
      ;; (advice-add 'org-refile :after #'org-save-all-org-buffers)
      ;; (advice-add 'org-deadline :after #'org-save-all-org-buffers)
      ;; (advice-add 'org-schedule :after #'org-save-all-org-buffers)
      ;; (advice-add 'org-todo :after #'org-save-all-org-buffers)
      ;; (advice-add 'org-agenda-quit :before #'org-save-all-org-buffers)
      ;; (advice-add 'org-agenda-deadline :after #'org-save-all-org-buffers)
      ;; (advice-add 'org-agenda-schedule :after #'org-save-all-org-buffers)
      (advice-add 'org-agenda-todo :after
		  (lambda (&rest _)
		      (org-save-all-org-buffers)))
      
      
      
      (setq org-todo-keywords
	      '((sequence "TODO(t)" "SOMEDAY(s)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
      ;; Configure custom agenda views
      (setq org-agenda-custom-commands
	  '(("W" "Work Tasks" tags "+Au"))))
      
      
      (use-package org-bullets
      :hook (org-mode . org-bullets-mode)
      :custom
      (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

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
