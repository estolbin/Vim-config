;; -*- lexical-binding: t; -*-
(load-theme 'monokai t)
(setq frame-title-format "emacs")

(require 'package)
(customize-set-variable 'package-archives
                        `(("melpa" . "https://melpa.org/packages/")
                          ,@package-archives))
(customize-set-variable 'package-enable-at-startup nil)
(package-initialize)

;; install use-package, if not installed
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; set another directory to backups
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; save disk space from backup
;;(setq delete-old-versions -1)
;;(setq version-control t)
;;(setq vc-make-backup-files t)
;;(setq auto-save-file-name-transforms '((".*" "~/emacs.d/auto-save-list" t)))

;; set shot version "yes" and "no"
(fset 'yes-or-no-p 'y-or-n-p)

(use-package quelpa
  :ensure t
  :defer t
  :custom
  (quelpa-update-melpa-p nil "Don't update the MELPA git repo."))

(use-package quelpa-use-package
  :ensure t)

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

(use-package emacs
  :load-path "secrets"
  :init
  (put 'narrow-to-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  (fset 'x-popup-menu #'ignore)
  :custom
  (frame-resize-pixelwise t)
  (default-frame-alist '((menu-bar-lines 0)
                         (tool-bar-lines 0)
                         (vertical-scroll-bars)))
  (scroll-step 1)
  (inhibit-startup-screen t "Don't show splash screen")
  (use-dialog-box nil "Disable dialog boxes")
  (x-gtk-use-system-tooltips nil)
  (use-file-dialog nil)
  (enable-recursive-minibuffers t "Allow minibuffer commands in the minibuffer")
  (indent-tabs-mode nil "Spaces!")
  (tab-width 4)
  (debug-on-quit nil)
  :config
  ;; Terminal emacs doesn't have it
  (when (fboundp 'set-fontset-font)
    ;; a workaround for old charsets
    (set-fontset-font "fontset-default" 'cyrillic
                      (font-spec :registry "iso10646-1" :script 'cyrillic))
    ;; TODO: is it possible to not hardcode fonts?
    (set-fontset-font t 'symbol
                      (font-spec :family
                                 (if (eq system-type 'darwin)
                                     "Apple Color Emoji"
                                   "Symbola"))
                      nil 'prepend)))

(use-package files
  :hook
  (before-save . delete-trailing-whitespace)
  :custom
  (require-final-newline t)
  ;; backup settings
  (backup-by-copying t)
  (backup-directory-alist
   `((".*" . ,(locate-user-emacs-file "backups"))))
  (delete-old-versions t)
  (kept-new-versions 6)
  (kept-old-versions 2)
  (version-control t))

(use-package autorevert
  :defer 0.1)

(use-package mule
  :defer 0.1
  :config
  (prefer-coding-system 'utf-8)
  (set-language-environment "UTF-8")
  (set-terminal-coding-system 'utf-8))

(use-package ispell
  :defer t
  :custom
  (ispell-local-dictionary-alist
   '(("russian"
      "[АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЬЫЪЭЮЯабвгдеёжзийклмнопрстуфхцчшщьыъэюя’A-Za-z]"
      "[^АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЬЫЪЭЮЯабвгдеёжзийклмнопрстуфхцчшщьыъэюя’A-Za-z]"
      "[-']"  nil ("-d" "ru_RU,en_US") nil utf-8)))
  (ispell-program-name "hunspell")
  (ispell-dictionary "russian")
  (ispell-really-aspell nil)
  (ispell-really-hunspell t)
  (ispell-encoding8-command t)
  (ispell-silently-savep t))

(use-package flyspell
  :defer t
  :custom
  (flyspell-delay 1))

(use-package flyspell-correct-ivy
  :ensure t
  :bind (:map flyspell-mode-map
              ("C-c $" . flyspell-correct-at-point)))

(use-package font-lock
  :defer t
  :custom-face
  (font-lock-comment-face ((t (:inherit font-lock-comment-face :italic t))))
  (font-lock-doc-face ((t (:inherit font-lock-doc-face :italic t))))
  (font-lock-string-face ((t (:inherit font-lock-string-face :italic t)))))

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(global-undo-tree-mode)
(global-set-key (kbd "M-/") 'undo-tree-vizualize)
(global-set-key (kbd "C-M-z") 'switch-window)
(global-set-key (kbd "C->") 'ace-jump-mode)

(use-package gcmh
  :ensure t
  :demand t
  :config
  (gcmh-mode 1))

;; for speed-up
(use-package fnhh
  :quelpa
  (fnhh :repo "a13/fnhh" :fetcher github)
  :config
  (fnhh-mode 1))

(require 'powerline)
;;(powerline-center-theme)
;;(powerline-center-evil-theme)
;;(powerline-vim-theme)
;;(powerline-default-theme)

;;(setq powerline-default-separator 'wave)

(unless (package-installed-p 'evil)
  (package-install 'evil))

;; Enable Evil
(use-package evil
	     :ensure t
	     :init
         (setq evil-want-integration t)
         (setq evil-want-keybindings nil)
	     (setq evil-vsplit-window-right t)
	     (setq evil-split-window-below t)
	     (setq evil-want-C-i-jump nil)
	     (setq evil-want-C-u-scroll t)
         (setq evil-respect-visual-line-mode t)
         (setq evil-undo-system 'undo-tree)
	     :config
	     (evil-mode 1))

;; start menu
;; dependencies
(use-package page-break-lines)
(use-package all-the-icons)

(use-package dashboard
     :config
     (setq show-week-agenda-p t)
     (setq dashboard-items '((recents . 15) (agenda . 5)))
     (setq dashboard-set-heading-icons t)
     (setq dashboard-set-file-icons t)
     (setq dashboard-startup-banner 3)
     (dashboard-setup-startup-hook))

;; Optimization
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "Emacs ready in %s with %d garbage collections."
		     (format "%.2f seconds"
			     (float-time
			      (time-subtract after-init-time before-init-time)))
		     gcs-done)))


;; Org-mode
(global-font-lock-mode 1)

;; doom-modeline.... 
(use-package doom-modeline
             :ensure t
             :init (doom-modeline-mode 1)
             :custom ((doom-modeline-height 15)))


;;(require 'org)
(use-package calendar
  :defer t
  :custom
  (calendar-week-start-day 1))

(use-package org
  :defer t
  ;; to be sure we have the latest Org version
  ;; :ensure org-plus-contrib
  :hook
  ;;(org-mode . variable-pitch-mode)
  (org-mode . visual-line-mode)
  :custom
  (org-adapt-indentation t)
  (org-src-tab-acts-natively t))

;; Files
(setq org-directory "d:/myorg/")
;;(setq org-agenda-files (list "inbox.org" "agenda.org"))
(setq org-agenda-files
      (mapcar 'file-truename
	      (file-expand-wildcards "d:/myorg/*.org")))

(defun gtd-save-org-buffers()
  "Save 'org-agenda-files' buffers without user confirmation. See also 'org-save-all-org-buffers'"
  (interactive)
  (message "Saving org-agenda-files buffers...")
  (save-some-buffers t (lambda ()
			 (when (member (buffer-file-name) org-agenda-files)
			   t)))
  (message "Saving org-agenda-files buffers... done"))

(advice-add 'org-refile :after
	    (lambda (&rest _)
	      (gtd-save-org-buffers)))

;; Capture
(setq org-capture-templates
       `(("i" "Inbox" entry  (file "inbox.org")
        ,(concat "* TODO %?\n"
                 "/Entered on/ %U"))
	 ("m" "Meeting" entry (file+headline "agenda.org" "Future")
	  ,(concat "* %? :meeting:\n"
		   "<%<%Y-%m-%d %a %H:00>>"))
	 ("n" "Note" entry (file "notes.org")
	  ,(concat "* Note (%a)\n"
		   "/Entered on/ %U\n" "\n" "%?"))
	 ))


;; settings for caprure
(defun org-capture-inbox()
  (interactive)
  (call-interactively 'org-store-link)
  (org-capture nil "i"))

;; use full window for capture
(add-hook 'org-capture-mode-hook 'delete-other-windows)

;; set key command
(define-key global-map (kbd "C-c c") 'org-capture)
(define-key global-map (kbd "C-c i") 'org-capture-inbox)
(define-key global-map (kbd "C-c a") 'org-agenda)


;; Refile
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-targets
      '(("projects.org" :regexp . "\\(?:\\(?:Note\\|Task\\)s\\)")))


;; TODO
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "HOLD(h@/!)" "|" "DONE(d)")))

(defun log-todo-next-creation-date (&rest ignore)
  "Log NEXT creation time in the property drawer under the key 'ACTIVATED'"
  (when (and (string= (org-get-todo-state) "NEXT")
	     (not (org-entry-get nil "ACTIVATED")))
    (org-entry-put nil "ACTIVATED" (format-time-string "[%Y-%m-%d]"))))
(add-hook 'org-after-todo-state-change-hook #'log-todo-next-creation-date)

;; Agenda
(setq org-agenda-include-diary t)
;;(setq org-agenda-custom-commands
      ;;'(("g" "Get Things Done (GTD)"
	 ;;((agenda ""
		  ;;((org-agenda-skip-function
		    ;;'(org-agenda-skip-entry-if 'deadline))
		   ;;(org-deadline-warning-days 0)))
	  ;;(todo "NEXT"
		;;((org-agenda-skip-function
		  ;;'(org-agenda-skip-entry-if 'deadline))
		 ;;(org-agenda-prefix-format "  %i %-12:c [%e] ")
		 ;;(org-agenda-overriding-header "\nTasks:\n")))
	  ;;(agenda nil
		  ;;((org-agenda-entry-types '(:deadline))
		   ;;(org-agenda-format-date "")
		   ;;(org-deadline-warning-days 7)
		   ;;(org-agenda-skip-function
		    ;;'(org-agenda-skip-entry-if 'notregexp "\\* NEXT"))
		   ;;(org-agenda-overriding-header "\nDeadlines")))
	  ;;(tags-todo "inbox"
		     ;;((org-agenda-prefix-format "  %?-12t% s")
		      ;;(org-agenda-overriding-header "\nInbox\n")))
	  ;;(tags "CLOSED>=\"<today>\""
;;((org-agenda-overriding-header "\nCompleted today\n")))))))

(setq org-agenda-custom-commands
      '(("d" "Today's Tasks"
	 ((tags-todo
	   "Sprint+Active=\"A\""
	   ((org-agenda-files '("d:/myorg/goals.org"))
	    (org-agenda-overriding-header "Цели на текущий спринт")))
	  (agenda "" ((org-agenda-span 1)
		      (org-agenda-overriding-header "Сегодня")))))
	("w" "Задачи на неделю"
	 ((tags-todo "Sprint+Active=\"A\""
		     ((org-agenda-files '("d:/myorg/goals.org"))
		      (org-agenda-overriding-header "Цели на текущий спринт")))
	  (agenda)))))
			
(setq org-agenda-hide-tags-regexp ".")

(setq org-agenda-prefix-format
      `((agenda . " %i %-12:c%?-12t% s")
	(todo   . " ")
	(tags   . " %i %-12:c")
	(search . " %i %-12:c")))


(setq org-log-done 'time)


;; evil mode for org
(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;;(setq org-default-notes-file (concat org-directory "/notes.org"))

;;(setq org-todo-keywords
;;      '(
;;	(sequence "IDEA(i)" "TODO(t)" "STARTED(s)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)")
;;	(sequence "|" "CANCELED(c)" "DELEGATED(l)" "SOMEDAY(f)")
;;	))

;;(setq org-todo-keywords-faces
;;      '(("IDEA" . (:foreground "GoldenRod" :weight bold))
;;	("NEXT" . (:foreground "IndianRed1" :weight bold))
;;	("STARTED" . (:foreground "OrangeRed" :weight bold))
;;	("WAITING" . (:foreground "coral" :weight bold))
;;	("CANCELED" . (:foreground "LimeGreen" :weight bold))
;;	("DELEGATED" . (:foreground "LimeGreen" :weight bold))
;;	("SOMEDAY" . (:foreground "LimeGreen" :weight bold))
;;	))

;;(setq org-todo-keywords
;;      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
;;	(sequence "|" "WAIT(w)" "BACK(b)")))

;;(setq org-todo-keyword-faces
;;      '(("NEXT" . (foreground "orange red" :weight bold))
;;	("WAIT" . (foreground "HotPink2" :weight bold))
;;	("BACK" . (foreground "MediumPurple3" :weight bold))))
	 
;; Agenda setup
(setq org-agenda-window-setup 'current-window)
;;(setq org-agenda-span 'day)


;;(setq org-fast-tag-selection-single-key t)
;;(setq org-use-fast-todo-selection t)

;;(setq org-reverse-note-order t)

;;(setq org-capture-templates
;;      '(("t" "Todo" entry (file+headline "d:/myorg/notes.org" "Tasks")
;;	 "* TODO %?\nAdded: %U\n" :prepend t :kill-buffer t)
;;	("i" "Idea" entry (file+headline "d:/myorg/notes.org" "Someday/Maybe")
;;	 "* IDEA %?\nAdded: %U\n" :prepend t :kill-buffer t)
;;	))

;;(setq org-src-fontify-natively t)
;;(setq org-src-tab-acts-natively t)


;; Custom Key Bindings
(global-set-key (kbd "<f12>") 'org-agenda)
(global-set-key (kbd "<f11>") 'org-clock-goto)

;; org-bullets
;;(require 'org-bullets)
;;(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(use-package org-bullets
  :ensure t
  :custom
  ;; org-bullets-bullet-list
  ;; default: "◉ ○ ✸ ✿"
  ;; large: ♥ ● ◇ ✚ ✜ ☯ ◆ ♠ ♣ ♦ ☢ ❀ ◆ ◖ ▶
  ;; Small: ► • ★ ▸
  (org-bullets-bullet-list '("•"))
  ;; others: ▼, ↴, ⬎, ⤷,…, and ⋱.
  ;; (org-ellipsis "⤵")
  (org-ellipsis "…")
  :hook
  (org-mode . org-bullets-mode))

(setq org-hide-emphasis-markers t
      org-fontify-done-headline t
      org-hide-leading-stars t
      org-pretty-entities t)
      
;; font-size in org mode
(defun my/org-mode-hook()
  "Stop the org-level headers from increasing in height relative to the other text."
  (dolist (face '(org-level-1
		  org-level-2
		  org-level-3
		  org-level-4
		  org-level-5))
    (set-face-attribute face nil :weight 'normal :height 1.0))
  )

(add-hook 'org-mode-hook #'my/org-mode-hook)

;; add some modules to org-mode
(setq org-modules '(org-habit))
(eval-after-load 'org
  '(org-load-modules-maybe t))


;; habits for org-mode
(setq org-habit-graph-column 80)
(setq org-habit-show-habits-only-for-today nil)

;; for cyrillic keyboard
(use-package unipunct-ng
  :quelpa
  (unipunct-ng
   :fetcher url
   :url "https://raw.githubusercontent.com/a13/xkb-custom/master/contrib/unipunct-ng.el"))

(use-package reverse-im
  :ensure t
  :demand t
  :after unipunct-ng char-fold
  :bind
  ("M-T" . reverse-im-translate-word)
  :custom
  (reverse-im-char-fold t)
  (reverse-im-read-char-advice-function #'reverse-im-read-char-exclude)
  (reverse-im-input-methods '("russian-unipunct-ng"))
  :config
  (reverse-im-mode t))


;; Store automatic customization options elsewhere
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

