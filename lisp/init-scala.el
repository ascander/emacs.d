;;; init-scala.el --- Part of my Emacs setup         -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Ascander Dost

;; Author: Ascander Dost <dostinthemachine@gmail.com>
;; Keywords: convenience, languages

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

;; This file contains settings for working in Scala.

;;; Code:

;; Major mode for editing Scala
(maybe-require-package 'scala-mode)

(after-load 'scala-mode
  ;; indentation preferences
  (setq scala-indent:default-run-on-strategy
        scala-indent:operator-strategy))

(defun ascander/scala-mode-newline-comments ()
  "Insert a leading asterisk in Scaladoc comments."
  (interactive)
  (newline-and-indent)
  (scala-indent:insert-asterisk-on-multiline-comment))

(after-load 'scala-mode
  (define-key scala-mode-map (kbd "RET") 'ascander/scala-mode-newline-comments)
  (define-key scala-mode-map (kbd "C-c m e") 'ensime))

;; Interactive support for Satan's Build Tool
(maybe-require-package 'sbt-mode)

;; disable smartparens mode in SBT buffers because it hangs while
;; trying to find matching delimiters
(add-hook 'sbt-mode-hook (lambda () (when (fboundp 'smartparens-mode)
                                 (smartparens-mode -1))))

;; ENhanced Scala Interaction Mode for Emacs
(maybe-require-package 'ensime)

(after-load 'ensime
  (setq ensime-startup-notification nil)
  (define-key ensime-mode-map (kbd "C-c m E") 'ensime-reload)
  (define-key ensime-mode-map (kbd "<f5>") 'ensime-sbt-do-compile))

(add-hook 'scala-mode-hook #'ensime-mode)

(provide 'init-scala)
;;; init-scala.el ends here
