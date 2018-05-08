;;; init-modeline.el --- Mode line configuration     -*- lexical-binding: t; -*-

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

;; Settings for my Emacs mode line.

;;; Code:

(use-package spaceline
  :ensure t
  :config
  (require 'spaceline-config)

  (validate-setq
   powerline-image-apple-rgb t
   powerline-default-separator 'slant
   powerline-height 20
   spaceline-minor-modes-separator nil
   spaceline-highlight-face-func 'spaceline-highlight-face-modified
   spaceline-separator-dir-left '(left . left)
   spaceline-separator-dir-right '(right . right)
   spaceline-buffer-encoding-abbrev-p nil
   spaceline-buffer-size-p nil)
  (spaceline-emacs-theme))

(provide 'init-modeline)
;;; init-modeline.el ends here
