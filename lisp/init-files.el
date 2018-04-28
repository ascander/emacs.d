;;; init-files.el --- File handling settings         -*- lexical-binding: t; -*-

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

;; This file contains settings for how Emacs handles files.

;;; Code:

;; Keep backup and auto-save files out of the way
(validate-setq
 backup-directory-alist `((".*" . ,(locate-user-emacs-file ".backup")))
 auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Delete files to the Trash
(validate-setq delete-by-moving-to-trash t)

;; View read-only files
(validate-setq view-read-only t)

;; UTF-8, only UTF-8, and nothing but UTF-8
(validate-setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (validate-setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

(use-package ignoramus			; Ignore uninteresting files everywhere
  :ensure t
  :config
  ;; Ignore some additional directories/files
  (dolist (name '("company-statistics-cache.el"
                  ".ensime_cache"
                  "ensime"))
    (add-to-list 'ignoramus-file-basename-exact-names name))
  (ignoramus-setup))

(use-package recentf			; save recently visited files
  :init (recentf-mode)
  :config
  (validate-setq recentf-max-saved-items 200
		 recentf-max-menu-items 15
		 ;; Cleanup recent files only when Emacs is idle, but
		 ;; not when the mode is enabled, because that slows
		 ;; down Emacs unnecessarily.
		 recentf-auto-cleanup 300
		 recentf-exclude (list "/\\.git/.*\\'" ; git contents
				       "/elpa/.*\\'"   ; package files
				       #'ignoramus-boring-p)))

(use-package neotree                    ; Tree view of projects
  :ensure t
  :bind (("C-c f t" . neotree-toggle))
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)
        neo-window-width 32
        neo-create-file-auto-open t
        neo-banner-message nil
        neo-show-updir-line nil
        neo-mode-line-type 'neotree
        neo-smart-open t
        neo-show-hidden-files t
        neo-auto-indent-point t))

(provide 'init-files)
;;; init-files.el ends here
