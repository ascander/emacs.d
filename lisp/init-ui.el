;;; init-ui.el --- Emacs UI settings                 -*- lexical-binding: t; -*-

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

;; This file contains settings for the Emacs UI. If it has to do with
;; how Emacs looks, it's in this file.

;;; Code:

;; Get rid of the tool bar and scroll bars
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Getting rid of the menu bar on OS X is problematic, so...
(if *is-a-mac*
    (add-hook 'after-make-frame-functions
	      (lambda (frame)
		(set-frame-parameter frame 'menu-bar-lines
				     (if (display-graphic-p frame) 1 0))))
  (when (fboundp 'menu-bar-mode)
    (menu-bar-mode -1)))

;; No blinking, ringing, startup screens or scratch messages.
(blink-cursor-mode -1)			
(validate-setq ring-bell-function #'ignore
               inhibit-startup-screen t
               initial-scratch-message nil)
(fset 'yes-or-no-p #'y-or-n-p)
;; Opt out from the startup message in the echo area by simply
;; disabling this ridiculously bizarre thing entirely.
(fset 'display-startup-echo-area-message #'ignore)

;; Fullscreen mode
(when (and *is-a-mac* (fboundp 'toggle-frame-fullscreen))
  ;; Command-Option-f to toggle fullscreen mode
  (global-set-key (kbd "M-ƒ") 'toggle-frame-fullscreen))

;; Opacity
(defun ascander/adjust-opacity (frame incr)
  "Adjust the background opacity of FRAME by increment INCR."
  (unless (display-graphic-p frame)
    (error "Cannot adjust opacity of this frame."))
  (let* ((oldalpha (or (frame-parameter frame 'alpha) 100))
	 (oldalpha (if (listp oldalpha) (car oldalpha) oldalpha))
	 (newalpha (+ incr oldalpha)))
    (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
      (modify-frame-parameters frame (list (cons 'alpha newalpha))))))

(global-set-key (kbd "M-C-8") (lambda () (interactive) (ascander/adjust-opacity nil -2)))
(global-set-key (kbd "M-C-9") (lambda () (interactive) (ascander/adjust-opacity nil 2)))
(global-set-key (kbd "M-C-0") (lambda () (interactive) (modify-frame-parameters nil `((alpha . 100)))))

;; Set frame title to (abbreviated) path of current buffer 
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; ---------------------------------------------------------------------------
;; UI packages
;; ---------------------------------------------------------------------------

(use-package dimmer			; highlight the current buffer
  :ensure t
  :config
  (add-hook 'after-init-hook 'dimmer-mode))

(use-package beacon			; highlight cursor in buffer
  :ensure t
  :init (beacon-mode 1)
  :diminish beacon-mode)

(use-package nlinum			; display line numbers in margin
  :ensure t
  :bind (("C-c t l" . nlinum-mode)))

(provide 'init-ui)
;;; init-ui.el ends here
