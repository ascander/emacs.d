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

;; Save all open file-visiting buffers, without prompting. Bind this to the
;; default 'C-x s' since you should probably save all buffers if you're saving
;; one of them. Additionally, adds a hook to save all on loss of focus. Taken
;; from: https://www.bytedude.com/useful-emacs-shortcuts/
(defun ad|save-open-buffers ()
  "Save all open file-visiting buffers without prompting. This
basically runs 'C-x s' on all open buffers."
  (interactive)
  (save-some-buffers t))

(global-set-key (kbd "C-x s") 'ad|save-open-buffers)

(add-hook 'focus-out-hook 'ad|save-open-buffers)

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

(use-package no-littering
  :ensure t
  :config
  ;; Versioning settings for old files
  (validate-setq
   create-lockfiles nil              ; don't use lockfiles
   delete-old-versions t             ; delete excessively old versions
   kept-new-versions 4               ; keep this many newest versions
   kept-old-versions 2               ; keep this many old versions
   version-control t)                ; use version control

  ;; Keep backup and auto-save files out of the way
  (validate-setq
   backup-directory-alist
   `((".*" . ,(no-littering-expand-var-file-name "backup/")))
   auto-save-file-name-transforms
   `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

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
                       #'ignoramus-boring-p))

  ;; Add `no-littering' directories to recentf
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  (add-to-list 'recentf-exclude no-littering-var-directory))

(use-package cus-edit                   ; Customize interface
  :ensure nil
  :config
  ;; Put `custom.el' in its place
  (validate-setq custom-file
                 (expand-file-name "custom.el" no-littering-etc-directory))
  ;; Miscellaneous other settings
  (validate-setq custom-buffer-done-kill nil
                 custom-buffer-verbose-help nil
                 custom-unlispify-tag-names nil
                 custom-unlispify-menu-entries nil)

  (load custom-file 'noerror 'nowarning))

;; Consider switching to 'dired' for this kind of thing.
(use-package neotree                    ; Tree view of projects
  :disabled t
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
