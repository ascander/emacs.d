;;; init-themes.el --- Color theme settings          -*- lexical-binding: t; -*-

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

;; This file contains color theme settings

;;; Code:

(use-package solarized-theme			; I always come back to you
  ;; :disabled t
  :ensure solarized-theme
  :config
  (validate-setq
   x-underline-at-descent-line t        ; draw the mode line underline lower
   solarized-use-variable-pitch nil	; no variable sized fonts
   solarized-use-less-bold t		; less bold
   solarized-use-more-italic t		; more italics
   solarized-distinct-doc-face t	; make docstrings stand out more
   solarized-emphasize-indicators nil	; less colors for gutters
   solarized-high-contrast-mode-line t  ; make active/inactive mode line stand out more
   ;; Avoid all font size changes
   solarized-height-minus-1 1.0
   solarized-height-plus-1 1.0
   solarized-height-plus-2 1.0
   solarized-height-plus-3 1.0
   solarized-height-plus-4 1.0)
  
  (load-theme 'solarized-dark 'no-confirm))

(use-package base16-theme		; base16 color themes
  :disabled t
  :ensure t
  :config
  (validate-setq base16-highlight-mode-line 'contrast)
  (load-theme 'base16-oceanicnext t))

;;------------------------------------------------------------------------------
;; Toggle between light and dark
;;------------------------------------------------------------------------------

(defun light ()
  "Activate a light color theme."
  (interactive)
  (load-theme 'solarized-light))

(defun dark ()
  "Activate a dark color theme."
  (interactive)
  (load-theme 'solarized-dark))

(provide 'init-themes)
;;; init-themes.el ends here
