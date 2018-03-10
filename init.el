;;; init.el --- Ascander Dost's Emacs configuration -*- lexical-binding: t; -*-
;; 
;; Copyright (C) 2018  Ascander Dost

;; Author: Ascander Dost
;; URL: https://github.com/ascander/emacs.d
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

;; This is my Emacs configuration, set up for (mostly) Scala
;; development, and forked from Steve Purcell's excellent setup at:
;; https://github.com/purcell/emacs.d .

;;; Code:

;; Debugging
(setq debug-on-error t)
(setq message-log-max 10000)

(defconst *spell-check-support-enabled* nil)   ; enable with `t'
(defconst *is-a-mac* (eq system-type 'darwin)) ; are we on a mac?

;;----------------------------------------------------------------------------
;; Adjust garbage collection thresholds during startup, and thereafter
;;----------------------------------------------------------------------------

(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'after-init-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;;----------------------------------------------------------------------------
;; Configure package management
;;----------------------------------------------------------------------------

;; Don't load old bytecode
(setq load-prefer-newer t)

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives
      ;; Package archives, the usual suspects
      '(("GNU ELPA"     . "http://elpa.gnu.org/packages/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/"))
      ;; Prefer MELPA Stable over GNU over MELPA.
      package-archive-priorities
      '(("MELPA Stable" . 10)
        ("GNU ELPA"     . 5)
        ("MELPA"        . 0)))

(package-initialize)

;; Install 'use-package' if necessary
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Enable 'use-package'
(eval-when-compile
  (require 'use-package))

;; ---------------------------------------------------------------------------
;; Customization, Environment, and OS settings
;; ---------------------------------------------------------------------------

(use-package validate                   ; validate options
  :ensure t)

(defconst *custom-file* (locate-user-emacs-file "custom.el")
  "File used to store settings from Customization UI.")

(use-package cus-edit                   ; customize interface
  :defer t
  :config
  (validate-setq custom-file *custom-file*
                 custom-buffer-done-kill nil
                 custom-buffer-verbose-help nil
                 custom-unlispify-tag-names nil
                 custom-unlispify-menu-entries nil)
  :init (load *custom-file* 'no-error 'no-message))

(use-package exec-path-from-shell
  :ensure t
  :if (and *is-a-mac* (display-graphic-p))
  :config
  (progn
    (when (string-match-p "/zsh$" (getenv "SHELL"))
      ;; Use a non-interactive login shell. This loads environment
      ;; variables from `.zprofile'.
      (validate-setq exec-path-from-shell-arguments '("-l")))

    (validate-setq exec-path-from-shell-variables
                   '("JAVA_OPTS"        ; Java options
                     "SBT_OPTS"         ; SBT options
                     "EMAIL"            ; My email address
                     "PATH"             ; Executables
                     "MANPATH"          ; Man pages
                     "INFOPATH"         ; Info directories
                     "LANG"             ; Language
                     "LC_CTYPE"         ; Character set
                     ))

    ;; Initialize Emacs' environment from the shell
    (exec-path-from-shell-initialize)

    ;; Tell Emacs who I am
    (validate-setq user-email-address (getenv "EMAIL"))

    ;; Re-initialize the `info-directory-list' from $INFOPATH. Since
    ;; `package.el' already initializes info, we need to explicitly
    ;; add the $INFOPATH directories to `info-directory-list'. Reverse
    ;; the list of info paths to prepend them in proper order.
    (with-eval-after-load 'info
      (dolist (dir (nreverse (parse-colon-path (getenv "INFOPATH"))))
        (when dir
          (add-to-list 'Info-directory-list dir))))))

;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------

(use-package init-osx                   ; my specific settings for OS X
  :if *is-a-mac*
  :load-path "lisp/")

;; (use-package init-ui                    ; general UI settings
;;   :load-path "lisp/")

;; (require-package 'wgrep)
;; (require-package 'diminish)
;; (require-package 'scratch)
;; (require-package 'command-log-mode)

;; (require 'init-frame-hooks)
;; (require 'init-xterm)
;; (require 'init-themes)
;; (require 'init-osx-keys)
;; (require 'init-gui-frames)
;; (require 'init-dired)
;; (require 'init-isearch)
;; (require 'init-grep)
;; (require 'init-uniquify)
;; (require 'init-ibuffer)
;; (require 'init-flycheck)

;; (require 'init-recentf)
;; (require 'init-smex)
;; (require 'init-ivy)
;; ;; (require 'init-helm)
;; (require 'init-hippie-expand)
;; (require 'init-company)
;; (require 'init-windows)
;; (require 'init-sessions)
;; (require 'init-fonts)
;; (require 'init-mmm)

;; (require 'init-editing-utils)
;; (require 'init-whitespace)

;; (require 'init-vc)
;; (require 'init-darcs)
;; (require 'init-git)
;; (require 'init-github)

;; (require 'init-projectile)

; (require 'init-compile)
; ;;(require 'init-crontab)
; (require 'init-textile)
; (require 'init-markdown)
; (require 'init-csv)
; (require 'init-erlang)
; (require 'init-javascript)
; (require 'init-php)
;; (require 'init-org)
;; (require 'init-scala)
; (require 'init-nxml)
; (require 'init-html)
; (require 'init-css)
; (require 'init-haml)
; (require 'init-http)
; (require 'init-python)
; (require 'init-haskell)
; (require 'init-elm)
; (require 'init-purescript)
; (require 'init-ruby)
; (require 'init-rails)
; (require 'init-sql)
; (require 'init-rust)
; (require 'init-toml)
; (require 'init-yaml)
; (require 'init-docker)
; (require 'init-terraform)
; ;;(require 'init-nix)
; (maybe-require-package 'nginx-mode)

; (require 'init-paredit)
; (require 'init-lisp)
; (require 'init-slime)
; (require 'init-clojure)
; (require 'init-clojure-cider)
; (require 'init-common-lisp)

; (when *spell-check-support-enabled*
;   (require 'init-spelling))

; (require 'init-misc)

; (require 'init-folding)
; (require 'init-dash)

; ;;(require 'init-twitter)
; ;; (require 'init-mu)
; (require 'init-ledger)
; ;; Extra packages which don't require any configuration

; (require-package 'gnuplot)
; (require-package 'lua-mode)
; (require-package 'htmlize)
; (require-package 'dsvn)
; (when *is-a-mac*
;   (require-package 'osx-location))
; (maybe-require-package 'regex-tool)
; (maybe-require-package 'dotenv-mode)

; (when (maybe-require-package 'uptimes)
;   (setq-default uptimes-keep-count 200)
;   (add-hook 'after-init-hook (lambda () (require 'uptimes))))

;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------
;; (require 'server)
;; (unless (server-running-p)
;;   (server-start))

;;----------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
;; (when (file-exists-p custom-file)
;;   (load custom-file))

;;----------------------------------------------------------------------------
;; Locales (setting them earlier in this file doesn't work in X)
;;----------------------------------------------------------------------------
;; (require 'init-locales)

;;----------------------------------------------------------------------------
;; Allow users to provide an optional "init-local" containing personal settings
;;----------------------------------------------------------------------------
;; (require 'init-local nil t)

(provide 'init)
;;; init.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; coding: utf-8
;; no-byte-compile: t
;; End:
