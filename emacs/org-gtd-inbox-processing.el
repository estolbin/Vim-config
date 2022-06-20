;;; org-gtd-inbox-processing.el --- Code to process inbox -*- lexical-binding: t; coding: utf-8 -*-
;;
;; Copyright © 2019-2021 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Inbox processing management for org-gtd.
;;
;;; Code:

(require 'transient)
(require 'org-gtd-core)
(require 'org-gtd-agenda)
(require 'org-gtd-projects)
(require 'org-gtd-refile)

;;;###autoload
(defvar org-gtd-process-map (make-sparse-keymap)
  "Keymap for command `org-gtd-process-mode', a minor mode.")

;;;###autoload
(define-minor-mode org-gtd-process-mode
  "Minor mode for org-gtd."
  nil " GPM" org-gtd-process-map
  :global nil
  (if org-gtd-process-mode
      (setq-local
       header-line-format
       (substitute-command-keys
        "\\<org-gtd-process-map>Clarify item.  Let Org GTD store it with `\\[org-gtd-choose]'."))
    (setq-local header-line-format nil)))

(defun my-org-gtd--refile ()
  "Org-refile function used in org-gtd-process-map"
  (interactive)
  (org-gtd--decorate-item)
  (org-refile)
  (org-gtd-process-inbox))

(defun my-org-gtd--reference ()
  "Org-roam-refile function used in org-grd-process-map"
  (interactive)
  (org-roam-refile)
  (org-gtd-process-inbox))

(transient-define-prefix org-gtd-choose ()
  "Choose how to categorize the current item.

Note that this function is intended to be used only during inbox processing.
Each action continues inbox processing, so you may put your emacs in an
undefined state."
  ["Actionable"
   [("q" "Quick action" org-gtd--quick-action)
    ("s" "Single action" org-gtd--single-action)]
   [("d" "Delegate" org-gtd--delegate)
    ("c" "Calendar" org-gtd--calendar)]
   [("p" "Project (multi-step)" org-gtd--project)
    ("P" "Refile to existing project" my-org-gtd--refile)]
   ]
  ["Non-actionable"
   [("i" "Incubate" org-gtd--incubate)
    ("a" "Archive this knowledge" org-gtd--archive)]
   [("r" "Reference" my-org-gtd--reference)
    ("t" "Trash" org-gtd--trash)]]
  ["Org GTD"
   ("x"
    "Exit. Stop processing the inbox for now."
    org-gtd--stop-processing)])

;;;###autoload
(defun org-gtd-process-inbox ()
  "Process the GTD inbox."
  (interactive)
  (set-buffer (org-gtd--inbox-file))
  (display-buffer-same-window (org-gtd--inbox-file) '())
  (delete-other-windows)

  (org-gtd-process-mode t)

  (condition-case err
      (progn
        (widen)
        (goto-char (point-min))
        (org-next-visible-heading 1)
        (org-back-to-heading)
        (org-narrow-to-subtree))
    (user-error (org-gtd--stop-processing))))

;;;###autoload
(defun org-gtd--archive ()
  "Process GTD inbox item as a reference item."
  (interactive)
  (org-todo "DONE")
  (with-org-gtd-context (org-archive-subtree))
  (org-gtd-process-inbox))

;;;###autoload
(defun org-gtd--project ()
  "Process GTD inbox item by transforming it into a project.
Allow the user apply user-defined tags from
`org-tag-persistent-alist', `org-tag-alist' or file-local tags in
the inbox.  Refile to `org-gtd-actionable-file-basename'."
  (interactive)

  (if (org-gtd--poorly-formatted-project-p)
      (org-gtd--show-error-and-return-to-editing)

  (org-gtd--decorate-item)
  (org-gtd-projects--nextify)
  (goto-char (point-min))
  (let ((org-special-ctrl-a t))
    (org-end-of-line))
  (insert " [/]")
  (org-update-statistics-cookies t)
  (org-gtd--refile org-gtd-projects)
  (org-gtd-process-inbox)))

(defun org-gtd--poorly-formatted-project-p ()
  "Return true if the project is composed of only one heading."
  (basic-save-buffer)
  (eql 1 (length (org-map-entries t))))

(defun org-gtd--show-error-and-return-to-editing ()
  "Tell the user something is wrong with the project."
  (display-message-or-buffer
   "A 'project' in GTD is a finite set of steps after which a given task is
complete. In Org GTD, this is defined as a top-level org heading with at least
one second-level org headings. When the item you are editing is intended to be
a project, create such a headline structure, like so:

* Project heading
** First task
** Second task
** Third task

If you do not need sub-headings, then make a single action instead.")
  (org-gtd-process-inbox))

;;;###autoload
(defun org-gtd--calendar ()
  "Process GTD inbox item by scheduling it.

Allow the user apply user-defined tags from
`org-tag-persistent-alist', `org-tag-alist' or file-local tags in
the inbox.  Refile to `org-gtd-actionable-file-basename'."
  (interactive)
  (org-gtd--decorate-item)
  (org-schedule 0)
  (org-gtd--refile org-gtd-calendar)
  (org-gtd-process-inbox))

;;;###autoload
(defun org-gtd--delegate ()
  "Process GTD inbox item by delegating it.
Allow the user apply user-defined tags from
`org-tag-persistent-alist', `org-tag-alist' or file-local tags in
the inbox.  Set it as a waiting action and refile to
`org-gtd-actionable-file-basename'."
  (interactive)
  (org-gtd--decorate-item)
  (org-gtd-delegate)
  (org-gtd--refile org-gtd-actions)
  (org-gtd-process-inbox))

;;;###autoload
(defun org-gtd--incubate ()
  "Process GTD inbox item by incubating it.
Allow the user apply user-defined tags from
`org-tag-persistent-alist', `org-tag-alist' or file-local tags in
the inbox.  Refile to any org-gtd incubate target (see manual)."
  (interactive)
  (org-gtd--decorate-item)
  (org-schedule 0)
  (org-gtd--refile org-gtd-incubated)
  (org-gtd-process-inbox))

;;;###autoload
(defun org-gtd--quick-action ()
  "Process GTD inbox item by doing it now.
Allow the user apply user-defined tags from
`org-tag-persistent-alist', `org-tag-alist' or file-local tags in
the inbox.  Mark it as done and archive."
  (interactive)
  (org-back-to-heading)
  (org-gtd--decorate-item)
  (org-todo "DONE")
  (with-org-gtd-context (org-archive-subtree))
  (org-gtd-process-inbox))

;;;###autoload
(defun org-gtd--single-action ()
  "Process GTD inbox item as a single action.
Allow the user apply user-defined tags from
`org-tag-persistent-alist', `org-tag-alist' or file-local tags in
the inbox.  Set as a NEXT action and refile to
`org-gtd-actionable-file-basename'."
  (interactive)
  (org-gtd--decorate-item)
  (org-todo "NEXT")
  (org-gtd--refile org-gtd-actions)
  (org-gtd-process-inbox))

;;;###autoload
(defun org-gtd--trash ()
  "Mark GTD inbox item as cancelled and archive it."
  (interactive)
  (org-gtd--decorate-item)
  (org-todo "CNCL")
  (with-org-gtd-context (org-archive-subtree))
  (org-gtd-process-inbox))

;;;###autoload
(defun org-gtd--stop-processing ()
  "Private function.

Stop processing the inbox."
  (interactive)
  (widen)
  (org-gtd-process-mode -1)
  (whitespace-cleanup))

(defun org-gtd--decorate-item ()
  "Apply hooks to add metadata to a given GTD item."
  (goto-char (point-min))
  (dolist (hook org-gtd-process-item-hooks)
    (save-excursion
      (save-restriction
        (funcall hook)))))

(provide 'org-gtd-inbox-processing)
;;; org-gtd-inbox-processing.el ends here
