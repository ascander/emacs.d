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

(use-package projectile			; project management for Emacs
  :ensure t
  :defer 1
  :config
  (projectile-global-mode)
  ;; Remove dead projects when Emacs is idle
  (run-with-idle-timer 10 nil #'projectile-cleanup-known-projects)
  (validate-setq
   projectile-completion-system 'ivy
   projectile-find-dir-includes-top-level t 
   projectile-switch-project-action #'projectile-dired)
  :diminish projectile-mode)

;; (use-package counsel-projectile		; counsel interface to projectile
;;   :ensure t
;;   :after projectile
;;   :config
;;   (counsel-projectile-on))

(use-package projectile-ripgrep		; ripgrep for projectile
  :ensure t
  :after projectile
  :bind ("C-c p G" . projectile-ripgrep))

(provide 'init-projectile)
;;; init-projectile.el ends here
