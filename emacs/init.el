(load-theme 'monokai t)
(setq frame-title-format "emacs")
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-default 'cursor-type 'hbar)
(ido-mode)
(column-number-mode)
(show-paren-mode)
(global-hl-line-mode)
(winner-mode t)
(windmove-default-keybindings)

;; install use-package, if not installed
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; set another directory to backups
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; save disk space from backup
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
;;(setq auto-save-file-name-transforms '((".*" "~/emacs.d/auto-save-list" t)))

;; set shot version "yes" and "no"
(fset 'yes-or-no-p 'y-or-n-p)

(require 'package)
(require 'use-package)

(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/")
	     t)

(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/")
	     t)

(package-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(ac-config-default)

(nlinum-mode)
(global-undo-tree-mode)
(global-set-key (kbd "M-/") 'undo-tree-vizualize)
(global-set-key (kbd "C-M-z") 'switch-window)
(global-set-key (kbd "C->") 'ace-jump-mode)

(powerline-center-theme)

(setq powerline-default-separator 'wave)

(unless (package-installed-p 'evil)
  (package-install 'evil))

;; Enable Evil
;;(require 'evil)
;;(evil-mode 1)
(use-package evil
	     :ensure t
	     :init
	     (setq evil-vsplit-window-right t)
	     (setq evil-split-window-below t)
	     (setq evil-want-C-i-jump nil)
	     (setq evil-want-C-u-scroll t)
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



(setq inhibit-splash-screen t)

;; utf 8
;;(set-language-environment "UTF-8")
;;(prefer-coding-system 'utf-8)
;;(set-default-coding-systems 'utf-8)
;;(set-terminal-coding-system 'utf-8)
;;(set-keyboard-coding-system 'utf-8)
;;(setq default-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

(transient-mark-mode 1)

(require 'org)

;; for org-mode
;;(global-set-key (kbd "C-c l") #'org-store-link)
;;(global-set-key (kbd "C-c a") #'org-agenda)
;;(global-set-key (kbd "C-c c") #'org-capture)
;;(global-set-key (kbd "C-c b") #'org-iswitchb)



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
      '((sequence "TODO(t)" "NEXT(n)" "HOLD(h)" "|" "DONE(d)")))

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
	   "Sprint+Active+PRIORITY=\"A\""
	   ((org-agenda-files '("d:/myorg/goals.org"))
	    (org-agenda-overriding-header "Цели на текущий спринт")))
	  (agenda "" ((org-agenda-span 1)
		      (org-agenda-overriding-header "Сегодня")))))
	("w" "Задачи на неделю"
	 ((tags-todo "Sprint+Active"
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
;;(setq org-agenda-window-setup 'current-window)
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

(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)


;; Custom Key Bindings
(global-set-key (kbd "<f12>") 'org-agenda)
(global-set-key (kbd "<f11>") 'org-clock-goto)

;; org-bullets
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

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


;;(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
;; '(package-selected-packages
;;   '(dashboard all-the-icons page-break-lines org-bullets undo-tree switch-window smex powerline nlinum monokai-theme evil ace-jump-mode ac-clang)))
;;(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
;; '(hl-line ((t (:extend t :background "#3C3D37" :height 1.0)))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(evil-org org-evil use-package undo-tree switch-window smex powerline page-break-lines org-bullets nlinum monokai-theme evil dashboard all-the-icons ace-jump-mode ac-clang)))
;;(custom-set-faces
;; ;; custom-set-faces was added by Custom.
;; ;; If you edit it by hand, you could mess it up, so be careful.
;; ;; Your init file should contain only one such instance.
;; ;; If there is more than one, they won't work right.
;; '(org-level-1 ((t (:inherit default :extend nil :foreground "#FD971F" :weight normal :height 1.0))))
;; '(org-level-2 ((t (:inherit default :extend nil :foreground "#A6E22E" :weight normal :height 1.0))))
;; '(org-level-3 ((t (:inherit default :extend nil :foreground "#66D9EF" :weight normal :height 1.0))))
;; '(org-level-4 ((t (:inherit default :extend nil :foreground "#E6DB74" :weight normal :height 1.0))))
;; '(org-level-5 ((t (:inherit default :extend nil :foreground "#A1EFE4" :weight normal :height 1.0))))
;; '(org-level-6 ((t (:inherit default :extend nil :foreground "#A6E22E")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
