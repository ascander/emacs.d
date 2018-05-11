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

;; Opacity
(defun ad|adjust-opacity (frame incr)
  "Adjust the background opacity of FRAME by increment INCR."
  (unless (display-graphic-p frame)
    (error "Cannot adjust opacity of this frame."))
  (let* ((oldalpha (or (frame-parameter frame 'alpha) 100))
     (oldalpha (if (listp oldalpha) (car oldalpha) oldalpha))
     (newalpha (+ incr oldalpha)))
    (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
      (modify-frame-parameters frame (list (cons 'alpha newalpha))))))

(global-set-key (kbd "M-C-8") (lambda () (interactive) (ad|adjust-opacity nil -2)))
(global-set-key (kbd "M-C-9") (lambda () (interactive) (ad|adjust-opacity nil 2)))
(global-set-key (kbd "M-C-0") (lambda () (interactive) (modify-frame-parameters nil `((alpha . 100)))))

;; Set frame title to (abbreviated) path of current buffer
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(use-package dimmer			; Highlight the current buffer
  :ensure t
  :config
  (add-hook 'after-init-hook 'dimmer-mode))

(provide 'init-ui)
;;; init-ui.el ends here
