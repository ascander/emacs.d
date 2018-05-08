;;; init-fonts.el --- Default font settings          -*- lexical-binding: t; -*-

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

;; This file contains font settings.

;;; Code:

;; Default fonts

(defvar default-font-size-pt 12
  "Default font size, in points.")

(set-face-attribute 'default nil
                    :family "Iosevka Type" :height 120)

(set-face-attribute 'variable-pitch nil
                    :family "Fira Sans" :height 130 :weight 'regular)

;; Global font resizing: https://github.com/kaushalmodi/.emacs.d
(defun ad|font-size-adj (scale &optional absolute)
  "Adjust font size globally: in all buffers, mode line, echo area, etc.

The built-in `text-scale-adjust' function does an excellent job
of font resizing, but it does not change font sizes outside the
current buffer; for example, in the mode line.

M-<SCALE> COMMAND increases font size by SCALE points if SCALE is positive
                  decreases font size by SCALE points if SCALE is negative
                  resets    font size if SCALE is 0.

If ABSOLUTE is non-nil, text scale is applied relative to the
default font size `default-font-size-pt'. Otherwise, the text
scale is applied relative to the current font size."
  (interactive "p")
  (if (= scale 0)
      (setq font-size-pt default-font-size-pt)
    (if (bound-and-true-p absolute)
        (setq font-size-pt (+ default-font-size-pt scale))
      (setq font-size-pt (+ font-size-pt scale))))
  ;; Internal font size is 10x font size in points.
  (set-face-attribute 'default nil :height (* font-size-pt 10)))

(defun ad|font-size-incr  () (interactive) (ad|font-size-adj +1))
(defun ad|font-size-decr  () (interactive) (ad|font-size-adj -1))
(defun ad|font-size-reset () (interactive) (ad|font-size-adj 0))

;; Initialize `font-size-pt'
(unless (boundp 'font-size-pt)
  (ad|font-size-reset))

(defhydra hydra-font-resize (:hint nil :color red)
  "Font Size"
  ("-" ad|font-size-decr "Decrease")
  ("=" ad|font-size-incr "Increase")
  ("0" ad|font-size-reset "Reset")
  ("RET" nil "quit" :color blue))

(bind-keys ("C-M-=" . hydra-font-resize/body))

(provide 'init-fonts)
;;; init-fonts.el ends here
