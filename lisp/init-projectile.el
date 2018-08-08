;;; init-projectile.el --- Project interaction settings          -*- lexical-binding: t; -*-

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

;; This file contains settings for Projectile.

;;; Code:

(use-package projectile			; Project management for Emacs
  :defer 2
  :config
  (projectile-global-mode)
  ;; Remove dead projects when Emacs is idle
  (run-with-idle-timer 10 nil #'projectile-cleanup-known-projects)
  (validate-setq
   projectile-completion-system 'ivy
   projectile-find-dir-includes-top-level t
   projectile-switch-project-action #'projectile-dired)
  :delight projectile-mode)

(use-package counsel-projectile		; Counsel interface to Projectile
  :pin melpa
  :after projectile
  :bind (("C-c p G" . counsel-projectile-rg))
  :config

  ;; Open project root in 'dired' instead of the default action when running
  ;; `counsel-projectile-switch-project'. Taken from:
  ;; https://github.com/ericdanan/counsel-projectile/issues/58#issuecomment-387752675
  (defun ad|counsel-projectile-switch-project-action-dired (project)
    "Open 'dired' at the root of a project."
    (let ((projectile-switch-project-action
           (lambda ()
             (projectile-dired))))
      (counsel-projectile-switch-project-by-name project)))

  (counsel-projectile-modify-action
   'counsel-projectile-switch-project-action
   '((add ("." ad|counsel-projectile-switch-project-action-dired
           "open 'dired' at the root of the project")
          1)))

  (counsel-projectile-mode 1))

(provide 'init-projectile)
;;; init-projectile.el ends here
