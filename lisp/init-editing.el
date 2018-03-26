;;; init-editing.el --- Basic file editing settings  -*- lexical-binding: t; -*-

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

;; This file contains settings for basic file editing.

;;; Code:

;; Match parentheses when you can
(when (fboundp 'electric-pair-mode)
  (add-hook 'after-init-hook 'electric-pair-mode))

(use-package smartparens                ; paired delimiters
  :ensure t
  :diminish smartparens-mode
  :config
  (require 'smartparens-config)
  (show-smartparens-global-mode t)

  (add-hook 'prog-mode-hook #'turn-on-smartparens-strict-mode)
  (add-hook 'markdown-mode-hook #'turn-on-smartparens-strict-mode)

  (sp-pair "(" ")"   :wrap "C-c (")
  (sp-pair "[" "]"   :wrap "C-c [")
  (sp-pair "{" "}"   :wrap "C-c {")
  (sp-pair "'" "'"   :wrap "C-c '")
  (sp-pair "\"" "\"" :wrap "C-c \""))

;; Basic preferences
(setq-default indent-tabs-mode nil	; disable tabs, but...
	      tab-width 8		; give them proper width
	      case-fold-search t	; ignore case when searching
	      column-number-mode t	; display column numbers
	      delete-selection-mode t	; typed text replaces selected region
              fill-column 100           ; bump fill column for modern displays
              sentence-end-double-space nil)

;; Prettify symbols when you can
(when (fboundp 'global-prettify-symbols-mode)
  (add-hook 'after-init-hook 'global-prettify-symbols-mode))

;; Newline behavior
(defun ascander/newline-at-end-of-line ()
  "Move to the end of the line, enter a newline, and reindent."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(global-set-key (kbd "M-<return>") 'ascander/newline-at-end-of-line)

(use-package autorevert			; auto-revert buffers of changed files
  :ensure t
  :init (global-auto-revert-mode)
  :config
  (validate-setq auto-revert-verbose nil
		 global-auto-revert-non-file-buffers t)
  (when *is-a-mac*
    (validate-setq auto-revert-use-notify nil))
  :diminish auto-revert-mode)

(provide 'init-editing)
;;; init-editing.el ends here
