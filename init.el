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

;; This configuration assumes Emacs 26 or higher.
;;
;; Making changes/testing:
;;
;; - Use M-x esup to profile startup time
;; - Use M-x restart-emacs to restart after making changes

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
  :when (and (memq window-system '(mac ns))
             (display-graphic-p))
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

;; ;; -----------------------------------------------------------------------------
;; ;; Customization, Environment, and OS settings
;; ;; -----------------------------------------------------------------------------

;; (use-package server                     ; Emacs server
;;   :ensure t
;;   :init (server-mode)
;;   :config (unless (server-running-p)
;;             (server-start)))

;; (use-package validate                   ; Validate options
;;   :ensure t)

;; (use-package paradox                    ; Modern Emacs package menu
;;   :ensure t
;;   :defer t
;;   :config
;;   (validate-setq
;;    paradox-column-width-package 32
;;    paradox-column-width-version 18
;;    paradox-execute-asynchronously t)
;;   (remove-hook 'paradox-after-execution-functions #'paradox--report-buffer-print))

;; (use-package restart-emacs              ; Restart Emacs from inside Emacs
;;   :defer t)

;; (use-package esup                       ; Profile Emacs startup from inside Emacs
;;   :disabled t
;;   :defer t)

;; ;; -----------------------------------------------------------------------------
;; ;; Initialize specific features
;; ;; -----------------------------------------------------------------------------

;; ;; Add files in my `.emacs.d/lisp' directory
;; (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; (use-package init-defaults)
;; (use-package init-osx)
;; (use-package init-ui)
;; (use-package init-modeline)
;; (use-package init-themes)
;; (use-package init-fonts)
;; (use-package init-files)
;; (use-package init-dired)
;; (use-package init-projectile)
;; (use-package init-org)
;; (use-package init-keys)
;; (use-package init-editing)
;; (use-package init-windows)
;; (use-package init-ivy)
;; (use-package init-hydra)
;; (use-package init-git)
;; (use-package init-prog)
;; (use-package init-company)
;; (use-package init-scala)
;; (use-package init-markdown)

(provide 'init)
;;; init.el ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (exec-path-from-shell use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; Local Variables:
;; indent-tabs-mode: nil
;; coding: utf-8
;; no-byte-compile: t
;; End:
