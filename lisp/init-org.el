;;; init-org.el --- Part of my Emacs setup           -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Ascander Dost

;; Author: Ascander Dost <dostinthemachine@gmail.com>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file contains `org-mode' settings.

;;; Code:

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; Additional link grabbing packages
(when *is-a-mac*
  (maybe-require-package 'grab-mac-link))
(maybe-require-package 'org-cliplink)

;; Fancy org bullets
(maybe-require-package 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; Keybindings
(define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c a") 'org-agenda)

;; Miscellaneous preferences
(setq org-log-done t
      org-fast-tag-selection-include-todo t
      org-use-fast-todo-selection t
      org-startup-truncated nil
      org-tags-column 80)

;; Default locations
(setq org-directory (expand-file-name "~/org")
      org-default-notes-file (concat org-directory "gtd.org")
      org-agenda-files '("~/org"))

;; TODO preferences
(setq org-todo-keywords
      '((sequence "IDEA(i)" "TODO(t)" "STARTED(s)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)")
        (sequence "|" "CANCELED(c)" "DELEGATED(l)" "SOMEDAY(f)")))

(setq org-tag-persistent-alist
      '((:startgroup . nil)
        ("HOME"      . ?h)
        ("WORK"      . ?k)
        ("PERSONAL"  . ?p)
        (:endgroup   . nil)
        (:startgroup . nil)
        ("PROJECT"   . ?r)
        ("BACKLOG"   . ?g)
        ("EXPLORE"   . ?x)
        ("OTHER"      .?o)
        (:endgroup   . nil)
        (:startgroup . nil)
        ("EASY"      . ?e)
        ("MEDIUM"    . ?m)
        ("HARD"      . ?a)
        (:endgroup   . nil)
        ("URGENT"    . ?u)
        ("KEY"       . ?k)
        ("BONUS"     . ?b)))

;; Disable priorities
(setq org-enable-priority-commands nil)

;; Capture settings
(global-set-key (kbd "C-c c") 'org-capture)

(setq org-reverse-note-order t)

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/refile.org" "Tasks")
         "* TODO %?\nAdded: %U\n" :prepend t :kill-buffer t)
        ("i" "Idea" entry (file+headline "~/org/refile.org" "Someday/Maybe")
         "* IDEA %?\nAdded: %U\n" :prepend t :kill-buffer t)
        ("h" "Home" entry (file+headline "~/org/refile.org" "Home")
         "* TODO %?\nAdded: %U\n" :prepend t :kill-buffer t)))

(provide 'init-org)
;;; init-org.el ends here
