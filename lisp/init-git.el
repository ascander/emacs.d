;;; init-git.el --- Git integration settings         -*- lexical-binding: t; -*-

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

;; Settings for Git and version control.

;;; Code:

(use-package diff-hl			; highlight diffs in the fringe
  :disabled t
  :ensure t
  :defer t
  :init
  (global-diff-hl-mode)				  ; always be highlighting
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode) ; in dired, too

  ;; Fall back to display margin if the fringe is unavailable
  (unless (display-graphic-p)
    (diff-hl-margin-mode))

  ;; Refresh `diff-hl' after Magit operations
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

(use-package magit                      ; the correct Git client
  :ensure t
  :bind (("C-c g c" . magit-clone)
         ("C-c g s" . magit-status)
         ("C-c g b" . magit-blame)
         ("C-c g l" . magit-log-buffer-line)
         ("C-c g p" . magit-pull))
  :config
  (validate-setq
   magit-save-repository-buffers 'dontask
   magit-refs-show-commit-count 'all
   magit-branch-prefer-remote-upstream '("master")
   magit-branch-adjust-remote-upstream-alist '(("origin/master" "master"))
   magit-revision-show-gravatars nil
   magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)

  ;; Show refined hunks during diffs
  (set-default 'magit-diff-refine-hunk t)

  ;; Set Magit's repo dirs for `magit-status' from Projectile's known
  ;; projects. Initialize the `magit-repository-directories'
  ;; immediately after Projectile was loaded, and update it every time
  ;; we switched projects, because the new project might have been
  ;; unknown before
  (defun ascander/magit-set-repo-dirs-from-projectile ()
    "Set `magit-repo-dirs' from known Projectile projects."
    (let ((project-dirs (bound-and-true-p projectile-known-projects)))
      ;; Remove trailing slashes from project directories, because
      ;; Magit adds trailing slashes again, which breaks the
      ;; presentation in the Magit prompt.
      (validate-setq magit-repository-directories
                     (mapcar #'directory-file-name project-dirs))))

  (with-eval-after-load 'projectile
    (ascander/magit-set-repo-dirs-from-projectile))

  (add-hook 'projectile-switch-project-hook
            #'ascander/magit-set-repo-dirs-from-projectile)

  ;; Refresh `magit-status' after saving a buffer
  (add-hook 'after-save-hook #'magit-after-save-refresh-status))

(use-package git-commit                 ; git commit message mode
  :ensure t
  :defer t
  :config
  ;; Oh, really?  Come on… I know what I'm doing…
  (remove-hook 'git-commit-finish-query-functions
               #'git-commit-check-style-conventions))

(use-package gitconfig-mode             ; git configuration mode
  :ensure t
  :defer t)

(use-package gitignore-mode             ; .gitignore mode
  :ensure t
  :defer t)

(use-package gitattributes-mode         ; git attributes mode
  :ensure t
  :defer t)

(use-package git-timemachine            ; go back in Git time
  :ensure t
  :bind (("C-c g t" . git-timemachine)))

(provide 'init-git)
;;; init-git.el ends here
