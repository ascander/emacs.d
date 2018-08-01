;;; init.el --- My Emacs configuration               -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Ascander Dost

;; Author: Ascander Dost <dostinthemachine@gmail.com>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

;; TODO: move this to a more reasonable place
;; Enter debugger on error, and keep more messages.
(setq debug-on-error t)
(setq message-log-max 10000)

;; Increase GC threshold for faster startup, and set to a more
;; reasonable value after startup.
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'after-init-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; Display timing information after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;;; Package management

(require 'package)
(setq-default
 load-prefer-newer t            ; prefer the newest version of a file
 package-enable-at-startup nil) ; explicitly initialize packages

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu"   . "http://elpa.gnu.org/packages/")
        ("org"   . "https://orgmode.org/elpa/")))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package t))

(eval-when-compile
  (require 'use-package))

;;; Environment settings

(use-package exec-path-from-shell       ; Fix $PATH on GUI Emacs (on Macs)
  :ensure t
  :when (memq window-system '(mac ns))
  :config
  ;; Don't check for environment variables in the wrong shell startup
  ;; file, and load some specific environment variables
  (setq exec-path-from-shell-check-startup-files nil
        exec-path-from-shell-variables
        '("JAVA_OPTS"                   ; Java options
          "SBT_OPTS"                    ; SBT options
          "PATH"                        ; Commands
          "MANPATH"                     ; Manpages
          "LANG"                        ; Language
          "LC_CTYPE"                    ; Character set
          ))
  (exec-path-from-shell-initialize))

(provide 'init)
;;; init.el ends here
