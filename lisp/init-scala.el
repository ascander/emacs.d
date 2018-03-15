;;; init-scala.el --- Scala development settings     -*- lexical-binding: t; -*-

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

;; This file contains settings for Scala development.

;;; Code:

(use-package scala-mode			; major mode for editing Scala
  :ensure t
  :defer t
  :config
  ;; Indentation preferences
  (validate-setq scala-indent:default-run-on-strategy
		 scala-indent:operator-strategy)

  ;; Newline behaves correctly in multiline comments
  (defun ascander/scala-mode-newline-comments ()
    "Insert a leading asterisk in multiline comments."
    (interactive)
    (newline-and-indent)
    (scala-indent:insert-asterisk-on-multiline-comment))

  (define-key scala-mode-map (kbd "RET")
    #'ascander/scala-mode-newline-comments))

(use-package sbt-mode			; interactive support for Satan's Build Tool
  :ensure t
  :defer t
  :bind (:map scala-mode-map
	      ("C-c m b c" . sbt-command)
	      ("C-c m b r" . sbt-run-previous-command))
  :config
  
  (defun ascander/scala-pop-to-sbt (new-frame)
    "Start an SBT REPL for this project, optionally in a NEW-FRAME.

Select the SBT REPL for the current project in a new window. If
  the REPL is not yet running, start it. With prefix argument,
  select the REPL in a new frame instead."
    (interactive "P")
    ;; Start SBT when not running, taken from `sbt:command'
    (when (not (comint-check-proc (sbt:buffer-name)))
      (sbt:run-sbt))

    (let ((display-buffer-overriding-action
	   (if new-frame '(display-buffer-pop-up-frame) nil)))
      (pop-to-buffer (sbt:buffer-name))))

  (with-eval-after-load 'scala-mode
    (bind-key "C-c m s" #'ascander/scala-pop-to-sbt scala-mode-map))

  ;; Disable smartparens mode in SBT buffers, because it frequently
  ;; hangs while trying ot find matching delimiters.
  (add-hook 'sbt-mode-hook (lambda () (when (fboundp 'smartparens-mode)
					(smartparens-mode -1)))))

(use-package ensime			; enhanced Scala interaction mode for Emacs
  :ensure t
  :after scala-mode
  :bind (:map ensime-mode-map
	      ("C-c m E" . ensime-reload)
	      ("<f5>" . ensime-sbt-do-compile)
	      :map scala-mode-map ("C-c m e" . ensime))
  :config
  ;; Shut up, Ensime
  (validate-setq ensime-startup-notification nil)
  
  ;; Enable Ensime for all Scala buffers
  (add-hook 'scala-mode-hook #'ensime-mode)
  )


(provide 'init-scala)
;;; init-scala.el ends here


