;;; init-keys.el --- Keys and keybindings for Emacs  -*- lexical-binding: t; -*-

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

;; Settings for keys and keybindings in Emacs.

;;; Code:

(use-package which-key                  ; show help popups for prefix keys
  :ensure t
  :delight which-key-mode " ⓦ"
  :init (which-key-mode)
  :config
  (validate-setq
   which-key-popup-type 'side-window
   which-key-side-window-location 'right
   which-key-idle-delay 0.4
   which-key-sort-order 'which-key-prefix-then-key-order
   which-key-replacement-alist
   '(
     ;; Replacements for how all or part of FUNCTION is replaced when
     ;; `which-key' displays:
     ;;
     ;;     KEY → FUNCTION
     ;;
     ;; Eg: after "C-c g" display "s → magit-status" as "s → git-status"
     ((nil . "Prefix Command")            . (nil . "prefix"))
     ((nil . "\\`\\?\\?\\'")              . (nil . "λ"))
     ((nil . "projectile-")               . (nil . "proj-"))
     ((nil . "magit-")                    . (nil . "git-"))
     ((nil . "\\`hydra-\\(.+\\)/body\\'") . (nil . "=|\\1")))))

(provide 'init-keys)
;;; init-keys.el ends here
