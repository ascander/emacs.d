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
  :ensure t
  :mode (("\\.org$" . org-mode))
  :bind (("C-c l"   . org-store-link)
	 ("C-c a"   . org-agenda)
	 ("C-c b"   . ascander/org-insert-checkbox)
         ("C-c c"   . org-capture))
  :config
  ;; Miscellaneous preferences
  (validate-setq
   org-src-fontify-natively t
   org-log-done 'time
   org-fast-tag-selection-include-todo t
   org-use-fast-todo-selection t
   org-startup-truncated nil
   org-tags-column 80)

  ;; TODO task states
  (setq
   org-todo-keywords
   '((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)")))

  ;; Agenda files
  (validate-setq org-agenda-files '("~/org/inbox.org"
                                    "~/org/gtd.org"
                                    "~/org/tickler.org"))

  ;; Capture settings
  (setq org-capture-templates '(("t" "Todo [inbox]" entry
                                 (file+headline "~/org/inbox.org" "Tasks")
                                 "** TODO %i%?")
                                ("T" "Tickler" entry
                                 (file+headline "~/org/tickler.org" "Tickler")
                                 "** %i%? \n %U")))  

  ;; Refile targets include non-inbox files (including `someday.org')
  (setq org-refile-targets '(("~/org/gtd.org" :maxlevel . 3)
                             ("~/org/someday.org" :level . 1)
                             ("~/org/tickler.org" :maxlevel . 2)))

  ;; Checkboxes
  (defun ascander/org-insert-checkbox ()
    "Insert a checkbox."
    (interactive)
    (insert "[ ] "))

  ;; Disable priorities
  (validate-setq org-enable-priority-commands nil)

  ;; Put new stuff at the top
  (validate-setq org-reverse-note-order t))

(use-package org-bullets		; fancy utf-8 bullets for org mode
  :defer t
  :init (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package org-cliplink            ; insert links from the clipboard
  :ensure t
  :bind ("C-c o i" . org-cliplink))

(provide 'init-org)
;;; init-org.el ends here

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
