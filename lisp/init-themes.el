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
  :ensure t
  :init
  :config
  (validate-setq
   ;; Fixes the box around the mode line
   x-underline-at-descent-line t
   solarized-use-variable-pitch nil
   solarized-use-less-bold t
   solarized-use-more-italic nil
   solarized-distinct-doc-face t
   solarized-emphasize-indicators nil
   solarized-high-contrast-mode-line t
   ;; Avoid all font size changes
   solarized-height-minus-1 1.0
   solarized-height-plus-1 1.0
   solarized-height-plus-2 1.0
   solarized-height-plus-3 1.0
   solarized-height-plus-4 1.0)

  (load-theme 'solarized-dark 'no-confirm))

(use-package base16-theme               ; Base16 color themes
  :ensure t
  :config
  (validate-setq base16-highlight-mode-line 'contrast))

(use-package doom-themes                ; DOOM themes
  :ensure t
  :config
  (progn
    (doom-themes-neotree-config)
    (doom-themes-org-config)))

(defun ad|reset-themes ()
  "Disable all currently enabled themes."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))

(defhydra hydra-color-theme (:hint nil :color amaranth)
  "
Color Theme:
_s_ Solarized Dark   _c_ DOOM City Lights
_S_ Solarized Light  _o_ DOOM One
^ ^                  _g_ DOOM Spacegrey
^ ^                  _n_ DOOM Nord
"
  ("s" (load-theme 'solarized-dark t))
  ("S" (load-theme 'solarized-light t))
  ("c" (load-theme 'doom-city-lights t))
  ("o" (load-theme 'doom-one t))
  ("g" (load-theme 'doom-spacegrey t))
  ("n" (load-theme 'doom-nord t))
  ("r" (ad|reset-themes) "reset")
  ("RET" nil "quit" :color blue))

(bind-keys ("C-c w t" . hydra-color-theme/body))

(provide 'init-themes)
;;; init-themes.el ends here
