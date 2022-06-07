(defvar efs/default-font-size 110)

;;(defvar org-dir "~/Dropbox/org/")
(load (expand-file-name "org-dir.el" user-emacs-directory))

(setq inhibit-startup-message t)
(setq auto-revert-verbose nil)
(setq global-auto-revert-mode t)

(setq nov-unzip-program (executable-find "unzip"))
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))


;; run server if not running
(cond ((eq system-type 'windows-nt)
        (require 'server)
        (unless (server-running-p)
          (server-start))))

(defvar emacs-autosave-directory
  (concat user-emacs-directory "autosaves/"))

(setq backup-directory-alist
      `((".*" . ,emacs-autosave-directory))
      auto-save-file-name-transforms
      `((".*" ,emacs-autosave-directory t)))

(setq undo-tree-history-directory-alist '((".*" . "~/.emacs.d/undo/")))
;;(setq
;; backup-by-copying t
;; backup-directory-alist '(("." . "~/.emacs.d/saves/"))
;; delete-old-versions t
;; kept-new-versions 6
;; kept-old-versions 2
;; version-control t)

(setq gc-cons-threshold (* 50 1000 1000))

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
	   (format "%.2f seconds"
		   (float-time
		    (time-subtract after-init-time before-init-time)))
	   gcs-done))
(add-hook 'emacs-startup-hook #'efs/display-startup-time)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(show-paren-mode 1)
(menu-bar-mode -1)

(setq visible-bell t)

(set-language-environment "Russian")
(prefer-coding-system 'utf-8)

(setq default-frame-alist '((font . "Fira Code")))
(set-face-attribute 'default nil :font "Fira Code" :height efs/default-font-size :weight 'regular)
(set-face-attribute 'fixed-pitch nil :font "Fira Code" :height efs/default-font-size :weight 'regular)
(set-face-attribute 'variable-pitch nil :font "Fira Code" :height efs/default-font-size :weight 'regular)


(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;(set-language-environment 'UTF-8)
(defalias 'yes-or-no-p 'y-or-n-p)

(require 'package)

;;("melpa" . "https://melpa.org/packages/")
(setq package-archives '(("melpa" . "http://www.mirrorservice.org/sites/melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(column-number-mode)
(global-display-line-numbers-mode t)

(dolist (mode '(org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
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
  :config
  (ivy-mode 1))

;;(global-set-key (kbd "C-M-j") 'counsel-switch-buffer)

(use-package all-the-icons)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package doom-themes
  :init (load-theme 'doom-nord t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :defer 0
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil))

(use-package general
  :after evil
  :config
  (general-create-definer efs/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (efs/leader-keys
   "t"  '(:ignore t :which-key "toggles")
   "tt" '(counsel-load-theme :which-key "choose theme")
   "oa" 'org-agenda-list
   "oA" 'org-agenda
   "oc" 'org-capture))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package hydra
  :defer t)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "int")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(efs/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

;; start week on monday
(setq calendar-week-start-day 1)

;; ORG-MODE
(defun efs/org-font-setup ()
  (with-eval-after-load 'org-faces
  (font-lock-add-keywords 'org-mode
			'(("^ *\\([-]\\) "
			   (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•")))))) 

  
  (dolist (face '((org-level-1 . 1.0)
                  (org-level-2 . 1.0)
                  (org-level-3 . 1.0)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.0)
                  (org-level-6 . 1.0)
                  (org-level-7 . 1.0)
                  (org-level-8 . 1.0)))
    (set-face-attribute (car face) nil :height (cdr face) :weight 'regular))))

    ;; Ensure that anything that should be fixed-pitch in Org files appears that way
;;  (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
;;  (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
;;  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
;;  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
;;  (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
;;  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
;;  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
;;  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  ;;  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch))
  

(defun org-journal-find-location ()
  (org-journal-new-entry t)
  (unless (eq org-journal-file-type 'daily)
    (org-narrow-to-subtree))
  (goto-char (point-max)))

(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  :pin org
  :commands (org-capture org-agenda)
  :hook (org-mode . efs/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-hide-emphasis-markers t)
  (setq org-agenda-skip-function-global '(org-agenda-skip-entry-if 'todo 'done))
  (setq org-directory org-dir)
  (setq org-agenda-files (list org-dir))

  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)
  (setq org-tag-alist
	'((:startgroup .nil)
	  ("Дом" . ?h) ("Офис" . ?w) ("Улица" . ?s) ("Магазин" . ?b)
	  (:endgroup .nil)
	  (:startgroup .nil)
	  ("Звонок" . ?c) ("Встреча" . ?m) ("Купить" . ?b) 
	  (:endgroup .nil)))
  
  (setq org-todo-keywords
	'((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
	  (sequence "PROJ(p)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

  (setq org-todo-keyword-faces
	'(quote (("TODO" :foreground "tomato")
		("PROJ" :foreground "light state gray")
		("NEXT" :foreground "tomato")
		("WAIT" :foregorund "orange")
		("HOLD" :foreground "turquoise"))))
  
  (setq org-refile-targets '((nil :maxlevel . 3)
			     (org-agenda-files :maxlevel . 3)))
  (setq org-outline-path-comlete-in-steps nil)
  (setq org-refile-use-outline-path t)
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  
  ;; Configure custom agenda views
  (setq org-agenda-custom-commands
   '(("d" "Dashboard"
     ((agenda "" ((org-deadline-warning-days 7)))
      (todo "TODO"
        ((org-agenda-overriding-header "Next Tasks")))
      (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

    ("n" "Next Tasks"
     ((todo "TODO"
        ((org-agenda-overriding-header "Next Tasks")))))

    ("W" "Work Tasks" tags-todo "+work-email")

    ;; Low-effort next actions
    ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
     ((org-agenda-overriding-header "Low Effort Tasks")
      (org-agenda-max-todos 20)
      (org-agenda-files org-agenda-files)))))

  (setq org-capture-templates
    `(("t" "Tasks / Projects")
      ("tt" "Task" entry (file+olp ,(concat org-dir "inbox.org") "Inbox")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

      ("j" "Journal Entries")
      ("jj" "Journal entry" plain (function org-journal-find-location)
       "** %(format-time-string org-journal-time-format)%^{Title}\n%i%?"
       :jump-to-captured t :immediate-finish t)
      ("jm" "Meeting" entry
           (file+olp+datetree ,(concat org-dir "Journal.org"))
           "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 1)

      ("l" "Ledger")
      ("li" "Income" plain (file ,(concat org-dir "my-finance.ledger"))
				 "%(org-read-date) * %^{Поступление}
                                      Активы:%^{Счет}  %^{Сумма}
                                      Прибыль:Зарплата")
      ("lc" "Cash expenses" plain (file ,(concat org-dir "my-finance.ledger"))
       "%(org-read-date) * %^{Траты}
           Расходы:%^{Счет}   %^{Сумма}
           Активы:%^{Счет}")
      
      ("m" "Metrics Capture")
      ("mw" "Вес" table-line (file+olp ,(concat org-dir "Metrics.org") "Weight")
					       "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)
      ("mr" "Зарядка" table-line (file+headline ,(concat org-dir "Metrics.org") "Зарядка")
						"| %U | %^{Отжимания} | %^{Приседания} | %^{Подтягивания} | %^{Скручивания} | %^{Обратные скручивания} |" :kill-buffer t))))

(define-key global-map (kbd "C-c j")
  (lambda () (interactive) (org-capture nil "jj")))
  
(efs/org-font-setup)

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package org-journal
  :ensure t
  :defer t
  :after org
  :init
  (setq org-journal-prefix-key "C-c j")
  :config
  (setq org-journal-dir (expand-file-name "Journal/" org-dir)
	org-journal-date-format "%A, %d %B %Y"))

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
	visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

(defun efs/my-ledger-hook ()
  (lambda ()
    (setq-local tab-always-indent 'complete)
    (setq-local completion-cycle-threshold t)
    (setq-local ledger-complete-in-steps t)))

(use-package ledger-mode
  :ensure t
  :init
  (setq ledger-clear-whole-transaction 1)

  :config
  (add-to-list 'evil-emacs-state-modes 'ledger-report-mode)
;;  :hook (
  :mode "\\.dat\\'"
        "\\.ledger\\'")


;; my often use files
(global-set-key (kbd "<f5>") 'lawlist-bookmark)

(defun lawlist-bookmark (choice)
  "Choices for directories and files."
  (interactive "c[w]ork.org | [p]ersonal.org | [t]asks.org | [f]inanse.ledger | [i]nbox.org | [d]ocs")
  (cond
   ((eq choice ?d)
    (dired (file-name-as-directory org-dir)))
   ((eq choice ?w)
    (find-file (expand-file-name "Work.org" org-dir)))
   ((eq choice ?p)
    (find-file (expand-file-name "personal.org" org-dir)))
   ((eq choice ?t)
    (find-file (expand-file-name "tasks.org" org-dir)))
   ((eq choice ?f)
    (find-file (expand-file-name "my-finance.ledger" org-dir)))
   ((eq choice ?i)
    (find-file (expand-file-name "inbox.org" org-dir))))
    (message "Opened:  %s" (buffer-name))
   (t (message "Quit")))
    
(use-package char-fold
  :custom
  (char-fold-symmetric t)
  (search-default-mode #'char-fold-to-regexp))

(use-package reverse-im
  :ensure t
  :custom
  (reverse-im-input-methods '("russian-computer"))
  :config
  (reverse-im-mode t))

(setq-default
 org-babel-load-languages '((ledger . t)
			    (emacs-lisp . t)))

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install))

;; org-roam
(use-package org-roam
  :ensure t
  :demand t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (concat org-dir "RoamNotes/"))
  (org-roam-db-location (expand-file-name "org-roam.db" org-roam-directory))
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n g" . org-roam-graph)
	 ("C-c n i" . org-roam-node-insert)
	 ("C-c n c" . org-roam-capture)
	 ("C-c n I" . org-roam-node-insert-immediate)
	 ;; Dailies
	 ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (org-roam-db-autosync-mode))

(defun org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (cons arg args))
	(org-roam-capture-templates (list (append (car org-roam-capture-templates)
					  '(:immediate-finish t)))))
  (apply #'org-roam-node-insert args)))

(use-package org-noter
  :after pdf-tools 
  :ensure t
  :config
  (setq org-noter-notes-search-path (list (concat org-dir "Noter/"))
	org-noter-default-notes-file-name '("notes.org")))


(use-package org-ac
  :ensure t
  :init (progn
	  (require 'org-ac)
	  (org-ac/config-default)))


;; auto-complete
(use-package auto-complete
  :ensure t
  :init
  (progn
    (ac-config-default)
    (global-auto-complete-mode t)))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(global-auto-revert-mode t)
 '(ledger-reports
   '(("bal" "%(binary) -f %(ledger-file) bal")
     ("reg" "%(binary) -f %(ledger-file) reg")
     ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
     ("account" "%(binary) -f %(ledger-file) reg %(account)")))
 '(org-agenda-files
   '("~/Dropbox/org/Someday.org" "c:/Users/stolbin.es/Dropbox/org/Metrics.org" "c:/Users/stolbin.es/Dropbox/org/Tasks.org" "c:/Users/stolbin.es/Dropbox/org/finance.org" "c:/Users/stolbin.es/Dropbox/org/inbox.org" "c:/Users/stolbin.es/Dropbox/org/personal.org" "c:/Users/stolbin.es/Dropbox/org/work.org"))
 '(package-selected-packages
   '(evil-org djvu auto-complete org-pdfview nov ledger-mode org-roam reverse-im org-journal visual-fill-column org-bullets which-key use-package rainbow-delimiters ivy-rich hydra general evil-collection doom-themes doom-modeline counsel)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-block ((t (:inherit fixed-pitch :extend t :background "#373E4C"))))
 '(variable-pitch ((t (:weight regular :height 110 :foundry "outline" :family "Fira Code")))))
