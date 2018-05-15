;;; init-org.el --- Org mode configuration           -*- lexical-binding: t; -*-

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

(use-package org			; the almighty Org mode
  :ensure org-plus-contrib
  :pin org
  :mode (("\\.org$" . org-mode))
  :bind (("C-c l"   . org-store-link)
         ("C-c a"   . org-agenda)
         ("C-c c"   . org-capture))
  :config
  ;; Miscellaneous preferences
  (validate-setq
   org-src-fontify-natively t
   org-log-done 'time
   org-use-fast-todo-selection t
   org-startup-folded nil
   org-startup-truncated nil
   org-tags-column 80
   org-enable-priority-commands nil
   org-reverse-note-order t)

  ;; TODO task states
  (setq
   org-todo-keywords
   '((sequence "TODO(t)" "IN-PROGRESS(i)" "NEXT(n)" "WAITING(w@/!)" "|" "DONE(d)" "CANCELED(c@/!)")
     (sequence "IDEA(e)")))

  ;; Common TODO tags
  (setq org-tag-alist '((:startgroup . nil)
                        ("@work" . ?w) ("@home" . ?h)
                        (:endgroup . nil)
                        ("project" . ?p)))

  ;; Don't allow changes of TODO state unless it makes sense
  (validate-setq org-enforce-todo-dependencies t
                 org-enforce-todo-checkbox-dependencies t)

  ;; Agenda files
  (validate-setq org-agenda-files '("~/org/inbox.org"
                                    "~/org/gtd.org"
                                    "~/org/emacs.org"
                                    "~/org/tickler.org"))

  ;; Capture settings
  (setq org-capture-templates '(("t" "Todo [inbox]" entry
                                 (file+headline "~/org/inbox.org" "Tasks")
                                 "** TODO %i%?")
                                ("i" "Idea [inbox]" entry
                                 (file+headline "~/org/inbox.org" "Ideas")
                                 "** IDEA %i%?")
                                ("T" "Tickler" entry
                                 (file+headline "~/org/tickler.org" "Tickler")
                                 "** %i%? \n %U")))

  ;; Refile targets include non-inbox files (including `someday.org')
  (setq org-refile-targets '(("~/org/gtd.org" :maxlevel . 2)
                             ("~/org/emacs.org" :maxlevel . 2)
                             ("~/org/someday.org" :level . 1)
                             ("~/org/tickler.org" :maxlevel . 2)))

  ;; Archiving
  (validate-setq org-archive-location "~/org/archive.org::* From %s")

  ;; Navigate by headings, using Ivy
  (validate-setq org-goto-interface 'outline-path-completion
                 org-outline-path-complete-in-steps nil))

(use-package org-bullets		; fancy utf-8 bullets for org mode
  :ensure t
  :defer t
  :init (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package org-cliplink            ; insert links from the clipboard
  :ensure t
  :bind ("C-c o i" . org-cliplink))

(use-package toc-org                    ; Maintain a ToC in org files
  :ensure t
  :defer t
  :init (add-hook 'org-mode-hook (lambda () (toc-org-enable))))

(provide 'init-org)
;;; init-org.el ends here

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
