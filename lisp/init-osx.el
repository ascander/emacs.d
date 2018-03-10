;;; init-osx.el --- OS X configuration settings       -*- lexical-binding: t; -*-

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

;; This file contains specific settings for OS X.

;;; Code:

;; Keybindings
(when *is-a-mac*
  (validate-setq mac-option-modifier 'none          ; the default 'meta' key
		 mac-command-modifier 'meta	    ; but this is so much easier to use
		 mac-right-command-modifier 'super  ; in case you need this key
		 mac-function-modifier 'hyper))	    ; in case you need this key

;; TODO - keep or remove this
;; (when *is-a-mac*
;;   ;; Make mouse wheel / trackpad scrolling less jerky
;;   (setq mouse-wheel-scroll-amount '(1
;;                                     ((shift) . 5)
;;                                     ((control))))
;;   (dolist (multiple '("" "double-" "triple-"))
;;     (dolist (direction '("right" "left"))
;;       (global-set-key (read-kbd-macro (concat "<" multiple "wheel-" direction ">")) 'ignore)))
;;   (global-set-key (kbd "M-`") 'ns-next-frame)
;;   (global-set-key (kbd "M-h") 'ns-do-hide-emacs)
;;   (global-set-key (kbd "M-˙") 'ns-do-hide-others)
;;   (after-load 'nxml-mode
;;     (define-key nxml-mode-map (kbd "M-h") nil))
;;   (global-set-key (kbd "M-ˍ") 'ns-do-hide-others)) ; what describe-key reports for cmd-option-h


(use-package ns-win                     ; OS X window support 
  :defer t
  :if *is-a-mac*
  :config
  ;; Don't pop up new frames from the workspace
  (validate-setq ns-pop-up-frames nil))

(use-package osx-trash                  ; OS X Trash support
  :if *is-a-mac*
  :ensure t
  :init (osx-trash-setup))

(provide 'init-osx)
;;; init-osx.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; coding: utf-8
;; End:
