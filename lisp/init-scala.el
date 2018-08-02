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
  (defun ad|scala-mode-newline-comments ()
    "Insert a leading asterisk in multiline comments."
    (interactive)
    (newline-and-indent)
    (scala-indent:insert-asterisk-on-multiline-comment))

  (define-key scala-mode-map (kbd "RET")
    #'ad|scala-mode-newline-comments))

(use-package sbt-mode			; interactive support for Satan's Build Tool
  :ensure t
  :defer t
  :bind (:map scala-mode-map
	          ("C-c m b c" . sbt-command)
	          ("C-c m b r" . sbt-run-previous-command))
  :config

  (defun ad|scala-pop-to-sbt (new-frame)
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
    (bind-key "C-c m s" #'ad|scala-pop-to-sbt scala-mode-map))

  ;; Disable smartparens mode in SBT buffers, because it frequently hangs while
  ;; trying to find matching delimiters.
  (add-hook 'sbt-mode-hook (lambda () (when (fboundp 'smartparens-mode)
					               (smartparens-mode -1)))))

(use-package ensime			; enhanced Scala interaction mode for Emacs
  :ensure t
  :after scala-mode
  :diminish (ensime-mode . " Ⓔ")
  :bind (:map ensime-mode-map
              ("C-c m E" . ensime-reload)
              ("C-c m x" . ensime-disconnect)
	          ("<f5>" . ensime-sbt-do-compile)
	          :map scala-mode-map ("C-c m e" . ensime))
  :config
  ;; Shut up, Ensime
  (validate-setq ensime-startup-notification nil)

  ;; Enable Ensime for all Scala buffers
  (add-hook 'scala-mode-hook #'ensime-mode)

  ;; Redefine semantic highlighting faces; this is because things like implicit
  ;; conversion use both underlining and an indication in the gutter; only one
  ;; is needed. Also, italics are for the weak-minded, so don't use them here,
  ;; or anywhere else if you can help it.
  ;;
  ;; See: https://github.com/ensime/ensime-server/issues/1036
  (setq
   ensime-sem-high-faces
   '((var                . scala-font-lock:var-face)
     (val                . (:inherit font-lock-constant-face))
     (varField           . scala-font-lock:var-face)
     (valField           . (:inherit font-lock-constant-face))
     (functionCall       . font-lock-function-name-face)
     (operator           . font-lock-keyword-face)
     (class              . font-lock-type-face)
     (trait              .  (:inherit font-lock-type-face))
     (object             . font-lock-constant-face)
     (package            . font-lock-preprocessor-face)
     (implicitConversion . nil)
     (implicitParams     . nil)
     (deprecated         . (:strike-through "dark gray"))))

  ;; Workaround for Yasnippet in `ensime-mode'. See comments in
  ;; https://github.com/ensime/ensime-emacs/issues/474 for discussion.
  (defun ad|yas-advise-indent-function (function-symbol)
    (eval `(defadvice ,function-symbol (around yas-try-expand-first activate)
             ,(format
               "Try to expand a snippet before point, then call `%s' as usual"
               function-symbol)
             (let ((yas-fallback-behavior nil))
               (unless (and (interactive-p)
                            (yas-expand))
                 ad-do-it)))))
  (ad|yas-advise-indent-function 'ensime-company-complete-or-indent))

(provide 'init-scala)
;;; init-scala.el ends here