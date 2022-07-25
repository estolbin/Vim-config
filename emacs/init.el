(defvar my/init-start-time (current-time) "Time when init.el was started")
(defvar my/section-start-time (current-time) "Time when section was started")

(setq debug-on-error t)

(setq visible-bell t)
(setq straight-check-for-modifications nil)

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



(setq bmw/face-height-default
      (if (eq system-type 'darwin)
          180
        110))

(set-face-attribute 'default t
                    ;:background "#000000"
                    ;:foreground "#ffffff"
                    ;:family "Fira Code"
                    :family "DejaVu Sans Mono"
                    ;:family "Roboto Mono"
                    :height bmw/face-height-default)

;;(setq nano-font-family-monospaced "Fira Code")
(setq nano-font-family-monospaced "DejaVu Sans Mono")
;(setq nano-font-family-monospaced "Roboto Mono")
(setq nano-font-size (/ bmw/face-height-default 10))

(straight-use-package 'use-package)

(use-package straight
  :custom
  (straight-use-package-by-default t))

(straight-use-package
 '(pdf-tools :type git :host github :repo "vedang/pdf-tools"))

;; NANO splash
(straight-use-package
 '(nano-splash :type git :host github :repo "rougier/nano-splash"))

;; NANO theme
(straight-use-package
 '(nano-theme :type git :host github :repo "rougier/nano-theme"))

;; NANO modeline
(straight-use-package
 '(nano-modeline :type git :host github :repo "rougier/nano-modeline"))

;; NANO agenda
(straight-use-package
 '(nano-agenda :type git :host github :repo "rougier/nano-agenda"))

;; SVG tags, progress bars & icons
(straight-use-package
 '(svg-lib :type git :host github :repo "rougier/svg-lib"))

;; Replace keywords with SVG tags
(straight-use-package
 '(svg-tag-mode :type git :host github :repo "rougier/svg-tag-mode"))

;;(defvar efs/default-font-size 110)

;;(defvar org-dir "~/Dropbox/org/")
;;(load (expand-file-name "org-dir.el" user-emacs-directory))
(let ((org-dir-file (expand-file-name "org-dir.el" user-emacs-directory)))
  (when (file-exists-p org-dir-file)
    (load-file org-dir-file)))

(setq-default
 inhibit-startup-screen t               ; Disable start-up screen
 inhibit-startup-message t              ; Disable startup message
 inhibit-startup-echo-area-message t    ; Disable initial echo message
 initial-scratch-message ""             ; Empty the initial *scratch* buffer
 initial-buffer-choice t)               ; Open *scratch* buffer at init


(set-default-coding-systems 'utf-8)     ; Default to utf-8 encoding
(prefer-coding-system       'utf-8)     ; Add utf-8 at the front for automatic detection.
(set-default-coding-systems 'utf-8)     ; Set default value of various coding systems
(set-terminal-coding-system 'utf-8)     ; Set coding system of terminal output
(set-keyboard-coding-system 'utf-8)     ; Set coding system for keyboard input on TERMINAL
(set-language-environment "Russian")    ; Set up multilingual environment

;;(setq inhibit-startup-message t)
;;(setq auto-revert-verbose nil)
;;(setq global-auto-revert-mode t)

(setq auto-save-list-file-prefix ; Prefix for generating auto-save-list-file-name
      (expand-file-name ".auto-save-list/.saves-" user-emacs-directory)
      auto-save-default t        ; Auto-save every buffer that visits a file
      auto-save-timeout 20       ; Number of seconds between auto-save
      auto-save-interval 200)    ; Number of keystrokes between auto-saves

(setq backup-directory-alist       ; File name patterns and backup directory names.
      `(("." . ,(expand-file-name "backups" user-emacs-directory)))
      make-backup-files t          ; Backup of a file the first time it is saved.
      vc-make-backup-files nil     ; No backup of files under version contr
      backup-by-copying t          ; Don't clobber symlinks
      version-control t            ; Version numbers for backup files
      delete-old-versions t        ; Delete excess backup files silently
      kept-old-versions 6          ; Number of old versions to keep
      kept-new-versions 9          ; Number of new versions to keep
      delete-by-moving-to-trash t) ; Delete files to trash

(setq bookmark-default-file (expand-file-name "bookmark" user-emacs-directory))

(defun unpropertize-kill-ring ()
  (setq kill-ring (mapcar 'substring-no-properties kill-ring)))

(add-hook 'kill-emacs-hook 'unpropertize-kill-ring)

(setq nov-unzip-program (executable-find "unzip"))
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

(require 'savehist)

(setq kill-ring-max 50
      history-length 50)

(setq savehist-additional-variables
      '(kill-ring
        command-history
        set-variable-value-history
        custom-variable-history   
        query-replace-history     
        read-expression-history   
        minibuffer-history        
        read-char-history         
        face-name-history         
        bookmark-history
        file-name-history))

 (put 'minibuffer-history         'history-length 50)
 (put 'file-name-history          'history-length 50)
 (put 'set-variable-value-history 'history-length 25)
 (put 'custom-variable-history    'history-length 25)
 (put 'query-replace-history      'history-length 25)
 (put 'read-expression-history    'history-length 25)
 (put 'read-char-history          'history-length 25)
 (put 'face-name-history          'history-length 25)
 (put 'bookmark-history           'history-length 25)

(setq history-delete-duplicates t)

(let (message-log-max)
  (savehist-mode))

(setq save-place-file (expand-file-name "saveplace" user-emacs-directory)
      save-place-forget-unreadable-files t)

(save-place-mode 1)

;; run server if not running
(cond ((eq system-type 'windows-nt)
        (require 'server)
        (unless (server-running-p)
          (server-start))))

(message "Core section time: %.2fs"
         (float-time (time-subtract (current-time) my/section-start-time)))

(setq my/session-start-time (current-time))

;;(defvar emacs-autosave-directory
;;  (concat user-emacs-directory "autosaves/"))

;;(setq backup-directory-alist
;;      `((".*" . ,emacs-autosave-directory))
;;      auto-save-file-name-transforms
;;      `((".*" ,emacs-autosave-directory t)))

(setq undo-tree-history-directory-alist '((".*" . "~/.emacs.d/undo/")))

;;(setq gc-cons-threshold (* 50 1000 1000))

;;(defun efs/display-startup-time ()
;;  (message "Emacs loaded in %s with %d garbage collections."
;;	   (format "%.2f seconds"
;;		   (float-time
;;		    (time-subtract after-init-time before-init-time)))
;;	   gcs-done))
;;(add-hook 'emacs-startup-hook #'efs/display-startup-time)


;;(set-language-environment "Russian")
;;(prefer-coding-system 'utf-8)

;;(setq default-frame-alist '((font . "Fira Code")))
;;(set-face-attribute 'default nil :font "Fira Code" :height efs/default-font-size :weight 'regular)
;;(set-face-attribute 'fixed-pitch nil :font "Fira Code" :height efs/default-font-size :weight 'regular)
;;(set-face-attribute 'variable-pitch nil :font "Fira Code" :height efs/default-font-size :weight 'regular)


(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;(set-language-environment 'UTF-8)
(defalias 'yes-or-no-p 'y-or-n-p)

;;(require 'package)

;;("melpa" . "https://melpa.org/packages/")
;;(setq package-archives '(("melpa" . "http://www.mirrorservice.org/sites/melpa.org/packages/")
;;			 ("org" . "https://orgmode.org/elpa/")
;;			 ("elpa" . "https://elpa.gnu.org/packages/")))

;;(package-initialize)
;;(unless package-archive-contents
;;  (package-refresh-contents))

;;(unless (package-installed-p 'use-package)
;;  (package-install 'use-package))

;;(require 'use-package)
;;(setq use-package-always-ensure t)

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

(straight-use-package
 '(nano-emacs :type git :host github :repo "rougier/nano-emacs"))
(require 'nano-layout)

(require 'nano-faces)
(nano-faces)

;;(setq nano-font-family-monospaced "Fira Code")
;;(setq nano-font-family-proportional nil)
;;(setq nano-font-size 11)
(require 'nano-theme)
(setq nano-fonts-use t)
(nano-theme)

(require 'nano-colors)
(material-color "deep-purple-2")
(open-color "grape-9")
(nord-color "aurora-0")

(require 'nano-defaults)
(require 'nano-session)
(require 'nano-modeline)

(provide 'nano)

(let ((inhibit-message t))
  (message "Welcome to GNU Emacs / N Λ N O edition")
  (message (format "Initialization time: %s" (emacs-init-time))))

(require 'nano-splash)

(require 'nano-help)

(provide 'nano)

;;(use-package doom-modeline
;;  :ensure t
;;  :init (doom-modeline-mode 1)
;;  :custom ((doom-modeline-height 15)))
;;
;;(use-package doom-themes
;;  :init (load-theme 'doom-nord t))

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
  (general-evil-setup)
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
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line))


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

  (add-to-list 'org-global-properties
               '("Effort_ALL" . "0:05 0:15 0:30 1:00 2:00 3:00 4:00"))
  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit t)
  (setq org-habit-graph-column 60)
  (setq org-tag-alist
	    '((:startgroup)
          ("@дом" . ?h)
          ("@офис" . ?w)
          ("@везде" . ?a)
          (:endgroup)
          (:startgroup)
          ("@звонок" . ?c)
          ("@встреча" . ?m)
          ("@купить" . ?b)
          (:endgroup)
          (:startgroup)
          ("Дениска" . ?D)
          ("Наташка" . ?N)
          (:endgroup)))
  
  (setq org-todo-keywords
	'((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
	  (sequence "WAIT(w@/!)" "HOLD(h)" "|" "CNCL(k@)")))

;;  (setq org-todo-keyword-faces
;;	'(("TODO" :foreground "tomato")
;;	  ("NEXT" :backgroung "medium sea green" :foreground "white" :weight bold)
;;	  ("WAIT" :foregorund "orange")))
;;  (setq org-todo-keyword-faces
;;        '(("TODO" :background "red1" :foreground "black" :weight bold :box (:line-width 2 :style released-button))))

;;  (set-face-attribute 'org-todo nil
;;                      :box '(:line-width 2
;;                             :color "grey75"
;;                             :style released-button)
;;                      :inverse-video t)
  
  (setq org-refile-targets '((nil :maxlevel . 3)
			     (org-agenda-files :maxlevel . 3)))
  (setq org-outline-path-comlete-in-steps nil)
  (setq org-refile-use-outline-path t)
;;  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  
  (setq org-capture-templates
    `(("t" "Tasks / Projects")
      ("tt" "Task" entry (file+olp ,(concat org-dir "inbox.org") "Inbox")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

      ("j" "Journal Entries")
      ("jj" "Journal entry" plain (function org-journal-find-location)
       "** %(format-time-string org-journal-time-format)%^{Title}\n%i%?"
       :jump-to-captured t :immediate-finish t)

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
      ("mr" "Зарядка" table-line (file+olp ,(concat org-dir "Metrics.org") "Зарядка")
						"| %U | %^{Отжимания} | %^{Приседания} | %^{Подтягивания} | %^{Скручивания} | %^{Обратные скручивания} |" :kill-buffer t))))

(define-key global-map (kbd "C-c j")
  (lambda () (interactive) (org-capture nil "jj")))
  
(efs/org-font-setup)

;;(use-package org-agenda
;;  :ensure nil
;;  :after org-gtd
;;  :custom
;;  (org-agenda-window-setup 'only-window))

(add-hook 'minibuffer-setup-hook 'my/minibuffer-setup)
(defun my/minibuffer-setup ()
  (set (make-local-variable 'face-remapping-alist)
                            '((default :height 0.9))))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init
   '(calendar
     dired
     ivy
     ibuffer
     pdf
     info
     flycheck
     bookmark)))

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
  (setq org-journal-file-type 'monthly)
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

;;(use-package ledger-mode
;;  :ensure t
;;  :init
;;  (setq ledger-clear-whole-transaction 1)
;;
;;  :config
;;  (add-to-list 'evil-emacs-state-modes 'ledger-report-mode)
;;;;  :hook (
;;  :mode "\\.dat\\'"
;;        "\\.ledger\\'")


;; my often use files
(global-set-key (kbd "<f5>") 'lawlist-bookmark)

(defun lawlist-bookmark (choice)
  "Choices for directories and files."
  (interactive "c[w]ork.org | [p]ersonal.org | [t]asks.org | [f]inanse.ledger | [i]nbox.org | [d]ocs")
  (cond
   ((eq choice ?d)
    (dired (file-name-as-directory org-dir)))
   ((eq choice ?w)
    (find-file (expand-file-name "Work.org" org-dir))
    (message "Opened:  %s" (buffer-name)))
   ((eq choice ?p)
    (find-file (expand-file-name "personal.org" org-dir))
    (message "Opened:  %s" (buffer-name)))
   ((eq choice ?t)
    (find-file (expand-file-name "tasks.org" org-dir))
    (message "Opened:  %s" (buffer-name)))
   ((eq choice ?f)
    (find-file (expand-file-name "my-finance.ledger" org-dir))
    (message "Opened:  %s" (buffer-name)))
   ((eq choice ?i)
    (find-file (expand-file-name "inbox.org" org-dir))
    (message "Opened:  %s" (buffer-name)))
   (t (message "Quit"))))
    
(use-package char-fold
  :custom
  (char-fold-symmetric t)
  (search-default-mode #'char-fold-to-regexp))

(use-package reverse-im
  :ensure t
  :demand t
  :after char-fold
  :bind
  ("M-T" . reverse-im-translate-word)
  :custom
  (reverse-im-char-fold t)
  (reverse-im-read-char-advice-function #'reverse-im-read-char-exclude)
  (reverse-im-input-methods '("russian-computer"))
  :config
  (reverse-im-mode t))

(setq-default
 ;; org-babel-load-languages '((ledger . t)
 org-babel-load-languages '((emacs-lisp . t)))

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :load-path "/usr/share/emacs/site-lisp/pfg-tools"
  :init
  (pdf-tools-install)
  :custom
  (pdf-view-display-size 'fit-heitht))
;;  :ensure t
;;  :config
;;  (pdf-tools-install))

;; org-roam
(use-package org-roam
  :ensure t
  :demand t
  :after org
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (concat org-dir "/RoamNotes/"))
  (org-roam-db-location (expand-file-name "org-roam.db" org-roam-directory))
  (org-roam-completion-everywhere t)
  :config
  (setq org-roam-mode-sections
        (list #'org-roam-backlinks-insert-section
              #'org-roam-reflinks-insert-section))
  (org-roam-setup)
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n g" . org-roam-graph)
	 ("C-c n i" . org-roam-node-insert)
	 ("C-c n c" . org-roam-capture)
	 ("C-c n I" . org-roam-node-insert-immediate)
	 ;; Dailies
	 ("C-c n j" . org-roam-dailies-capture-today)))

(add-to-list 'display-buffer-alist
             '("\\*org-roam\\*"
               (display-buffer-in-side-window)
               (side . right)
               (slot . 0)
               (window-width . 0.33)
               (window-parameters . ((no-other-window t)
                                     (no-delete-other-window . t)))))

(use-package org-roam-ui
  :straight
    (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
    :after org-roam
    ;; :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

(use-package org-gtd
  :straight
  (org-gtd type: git :host github :repo "trevoke/org-gtd.el")
  :after org
  :demand t
  :custom
  (org-gtd-directory org-dir)
  (org-edna-use-inheritance t)
  :config
  (org-edna-mode 1)
  :bind
  (("C-c d c" . org-gtd-capture)
   ("C-c d e" . org-gtd-engage)
   ("C-c d p" . org-gtd-process-inbox)
   ("C-c d n" . org-gtd-show-all-next)
   ("C-c d s" . org-gtd-show-stuck-projects)
   :map org-gtd-process-map
   ("C-c c" . org-gtd-choose)))


(defun org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (cons arg args))
	(org-roam-capture-templates (list (append (car org-roam-capture-templates)
					  '(:immediate-finish t)))))
  (apply #'org-roam-node-insert args)))

(use-package org-noter-pdftools
  :after org-noter
  :config
  ;; Add a function to ensure precise note is inserted
  (defun org-noter-pdftools-insert-precise-note (&optional toggle-no-questions)
    (interactive "P")
    (org-noter--with-valid-session
     (let ((org-noter-insert-note-no-questions (if toggle-no-questions
                                                   (not org-noter-insert-note-no-questions)
                                                 org-noter-insert-note-no-questions))
           (org-pdftools-use-isearch-link t)
           (org-pdftools-use-freestyle-annot 'org-pdftools-use-freepointer-annot))
       (org-noter-insert-note (org-noter--get-precise-info)))))

  ;; fix https://github.com/weirdNox/org-noter/pull/93/commits/f8349ae7575e599f375de1be6be2d0d5de4e6cbf
  (defun org-noter-set-start-location (&optional arg)
    "When opening a session with this document, go to the current location.
With a prefix ARG, remove start location."
    (interactive "P")
    (org-noter--with-valid-session
     (let ((inhibit-read-only t)
           (ast (org-noter--parse-root))
           (location (org-noter--doc-approx-location (when (called-interactively-p 'any) 'interactive))))
       (with-current-buffer (org-noter--session-notes-buffer session)
         (org-with-wide-buffer
          (goto-char (org-element-property :begin ast))
          (if arg
              (org-entry-delete nil org-noter-property-note-location)
            (org-entry-put nil org-noter-property-note-location
                           (org-noter--pretty-print-location location))))))))
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))

;;(use-package org-noter
;;  :after pdf-tools 
;;  :ensure t
;;  :config
;;  (setq org-noter-notes-search-path (list (concat org-dir "Noter/"))
;;	org-noter-default-notes-file-name '("notes.org")))
(use-package org-noter
  :after (:any org pdf-view org-noter-pdftools)
  :bind ((:map org-mode-map ("C-c o" . org-noter))
         (:map org-noter-notes-mode-map
               ("C-c k" . org-noter-pdftools-create-skeleton)
               ("C-c q" . org-noter-kill-session)))
  :custom
  (org-noter-always-create-frame nil)
  (org-noter-kill-frame-at-session nil)
  (org-noter-hide-other nil)
  :config
  (setq org-noter-notes-search-path (list (concat org-dir "Noter/"))
        org-noter-default-notes-file-name '("notes.org"))
  (require 'org-noter-pdftools))


(use-package org-ac
  :config
  (org-ac/config-default))

;; auto-complete
(use-package auto-complete
  :ensure t
  :init
  (progn
    (ac-config-default)
    (global-auto-complete-mode t)))


(straight-use-package
 '(org-imenu :type git :host github :repo "rougier/org-imenu"))

(use-package imenu-list)
;; imenu - sidebar
(require 'imenu)
(require 'imenu-list)

(defvar my/org-blocks-hidden nil)

(defun my/org-tree-to-indirect-buffer()
  "Create indirect buffer, narrow it to current subtree and unfold blocks"

  (org-tree-to-indirect-buffer)
  (org-show-block-all)
  (setq-local my/org-blocks-hidden nil))

(defun my/org-sidebar ()
  "Open a menu list on left that allow navigations"

  (interactive)
  (setq imenu-list-after-jump-hook #'my/org-tree-to-indirect-buffer
        imenu-list-position 'left
        imenu-list-size 36
        imenu-list-focus-after-activation t)

  (let ((heading (substring-no-properties (or (org-get-heading t t t t) "" ))))
    (when (buffer-base-buffer)
      (switch-to-buffer (buffer-base-buffer)))
    (imenu-list-minor-mode)
    (imenu-list-stop-timer)
    (hl-line-mode)
    (face-remap-add-relative 'hl-line :inherit 'nano-strong-i)
    (setq header-line-format
          '(:eval
            (nano-modeline-render nil
                                  (buffer-name imenu-list--displayed-buffer)
                                  "(outline)"
                                  "")))
    (setq-local cursor-type nil)
    (when (> (length heading) 0)
      (goto-char (point-min))
      (search-forward heading)
      (imenu-list-display-dwim))))

(defun my/org-sidebar-toggle ()
  "Toggle the org-sidebar"

  (interactive)
  (if (get-buffer-window "*Ilist*")
      (progn
        (quit-window nil (get-buffer-window "*Ilist*"))
        (switch-to-buffer (buffer-base-buffer)))
    (my/org-sidebar)))

(defvar my/imenu-list-folding-status t)

(defun my/imenu-list-toggle-folding ()
  "Toggle top level nodes of the imenu-list buffer"

  (interactive)
  (with-current-buffer "*Ilist*"
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\+ " nil t)
        (if my/imenu-list-folding-status
            (hs-hide-block)
          (hs-show-block)))
      (setq my/imenu-list-folding-status (not my/imenu-list-folding-status)))))
(bind-key "S-<tab>" #'my/imenu-list-toggle-folding imenu-list-major-mode-map)

(defun my/display-code-line-counts (ov)
  (when (eq 'code (overlay-get ov 'hs))
    (overlay-put ov 'display
                 (propertize
                  (format " [%d sections] … "
                          (- (count-lines (overlay-start ov)
                                       (overlay-end ov)) 1))
                  'face 'nano-faded))))

(setq hs-set-up-overlay #'my/display-code-line-counts)
(defun my/imenu-list-display-dwim ()
  "Display or toggle the entry at `point'."
  (interactive)
  (save-selected-window
    (save-excursion
      (my/imenu-list-ret-dwim))))

(defun my/imenu-list-ret-dwim ()
  "Jump to or toggle the entry at `point'."
  (interactive)
  (save-excursion
    (let ((entry (imenu-list--find-entry)))
      (when (imenu--subalist-p entry)
        (setq entry (cons
                     (car entry)
                     (get-text-property 0 'marker (car entry)))))
      (imenu-list--goto-entry entry))))

(bind-key "<SPC>" #'my/imenu-list-display-dwim imenu-list-major-mode-map)
(bind-key "<return>" #'my/imenu-list-ret-dwim imenu-list-major-mode-map)

(bind-key "C-c f" #'my/org-imenu-filter)
(bind-key "f" #'my/org-imenu-filter imenu-list-major-mode-map)
(bind-key "U" #'imenu-list-refresh imenu-list-major-mode-map)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(show-paren-mode 1)
(menu-bar-mode -1)
(winner-mode 1)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("5011dba8f3d9f310de6034905f2d47928473f8769fa8a37edc0ab5469fbb05f2" "b0b6bc4aef5dafbb4d191513645557dc8c79e20ffe0b6e4a5ca1ea3214b28bd2" default))
 '(global-auto-revert-mode t)
 '(ledger-reports
   '(("bal" "%(binary) -f %(ledger-file) bal")
     ("reg" "%(binary) -f %(ledger-file) reg")
     ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
     ("account" "%(binary) -f %(ledger-file) reg %(account)")))
 '(org-agenda-files
   '("c:/Users/stolbin.es/Dropbox/org/org-gtd-tasks.org" "c:/Users/stolbin.es/Dropbox/org/Archive.org" "c:/Users/stolbin.es/Dropbox/org/Metrics.org" "c:/Users/stolbin.es/Dropbox/org/Someday.org" "c:/Users/stolbin.es/Dropbox/org/Tasks.org" "c:/Users/stolbin.es/Dropbox/org/habits.org" "c:/Users/stolbin.es/Dropbox/org/inbox.org" "c:/Users/stolbin.es/Dropbox/org/personal.org" "c:/Users/stolbin.es/Dropbox/org/work.org"))
 '(package-selected-packages
   '(evil-org djvu auto-complete org-pdfview nov ledger-mode org-roam reverse-im org-journal visual-fill-column org-bullets which-key use-package rainbow-delimiters ivy-rich hydra general evil-collection doom-themes doom-modeline counsel)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
