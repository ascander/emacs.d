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

;;; OS X settings

;; Modifier keys
(when (eq system-type 'darwin)
      (setq mac-command-modifier 'meta
            mac-option-modifier 'super
            mac-function-modifier 'none))

(use-package osx-trash
  :ensure t
  :if (eq system-type 'darwin)
  :config
  (osx-trash-setup))

;;; Customization and packages

(use-package cus-edit                   ; The Customization UI
  :defer t
  :config
  (setq custom-file (locate-user-emacs-file "custom.el"))
  (load custom-file 'no-error 'no-message))

;;; Basic UI settings

;; Disable tool-bar, scroll-bar, and menu-bar. The menu bar cannot be
;; removed on OS X, so only remove it if you're not on a Mac.
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (and (not (eq system-type 'darwin))
           (fboundp 'menu-bar-mode))
  (menu-bar-mode -1))

;; Set some sensible defaults
(setq-default
 blink-cursor-mode -1                ; No blinking
 ring-bell-function #'ignore         ; No ringing
 inhibit-startup-screen t            ; No startup screen
 initial-scratch-message ""          ; No message in the scratch buffer
 cursor-in-non-selected-windows nil  ; Hide the cursor in inactive windows
 delete-by-moving-to-trash t         ; Delete files to trash
 fill-column 80                      ; Set width for modern displays
 help-window-select t                ; Focus new help windows when opened
 indent-tabs-mode nil                ; Stop using tabs to indent
 tab-width 4                         ; But set their width properly
 left-margin-width 0                 ; No left margin
 right-margin-width 0                ; No right margin
 recenter-positions '(12 top bottom) ; Set re-centering positions
 scroll-conservatively 1000          ; Never recenter point while scrolling
 select-enable-clipboard t           ; Merge system's and Emacs' clipboard
 sentence-end-double-space nil       ; Single space after a sentence end
 show-trailing-whitespace nil        ; Don't display trailing whitespaces by default
 split-height-threshold nil          ; Disable vertical window splitting
 split-width-threshold nil           ; Disable horizontal window splitting
 uniquify-buffer-name-style 'forward ; Uniquify buffer names correctly
 window-combination-resize t)        ; Resize windows proportionally

;; Miscellaneous settings
(fset 'yes-or-no-p 'y-or-n-p)                      ; Replace yes/no prompts with y/n
(fset 'display-startup-echo-area-message #'ignore) ; No startup message in the echo area
(delete-selection-mode 1)                          ; Replace region when inserting text
(put 'downcase-region 'disabled nil)               ; Enable downcase-region
(put 'upcase-region 'disabled nil)                 ; Enable upcase-region
(global-hl-line-mode)                              ; Highlight the current line
(line-number-mode)                                 ; Display line number in the mode line
(column-number-mode)                               ; Display column number in the mode line

;; Make the Emacs shell ('M-x shell') interactive, and disable echoing each
;; terminal command as it's entered on the command line.
(setq shell-command-switch "-ic")
(add-hook 'comint-mode-hook
          '(lambda () (setq comint-process-echoes t)))

;;; Fonts

(set-face-attribute 'default nil
                    :family "Iosevka Type"
                    :height 120
                    :weight 'regular)

(set-face-attribute 'variable-pitch nil
                    :family "Fira Sans"
                    :height 140
                    :weight 'regular)

;;; Color theme and looks

(use-package solarized-theme            ; I always come back to you
  :ensure t
  :init
  ;; Basic settings - disprefer bold and italics, use high contrast
  (setq x-underline-at-descent-line t   ; Fixes the box around the mode line
        solarized-use-variable-pitch nil
        solarized-use-less-bold t
        solarized-use-more-italic nil
        solarized-distinct-doc-face t
        solarized-emphasize-indicators nil
        solarized-high-contrast-mode-line t)
  ;; Avoid all font size changes
  (setq solarized-height-minus-1 1.0
        solarized-height-plus-1 1.0
        solarized-height-plus-2 1.0
        solarized-height-plus-3 1.0
        solarized-height-plus-4 1.0)
  :config
  (load-theme 'solarized-dark 'no-confirm))

(use-package dimmer                     ; Highlight the current buffer
  :ensure t
  :init
  (add-hook 'after-init-hook #'dimmer-mode))

(use-package stripe-buffer              ; Striped backgorund in `dired'
  :ensure t
  :init
  (add-hook 'dired-mode-hook #'turn-on-stripe-buffer-mode))

;;; Package manager and init development

(use-package paradox                    ; Modernizing Emacs' package menu
  :defer t
  :init
  (defconst *paradox-github-token-file*
    (concat user-emacs-directory "site-lisp/" "paradox-token.el")
    "Location of the `paradox-github-token' setting for Paradox.")
  :config
  ;; Basic settings
  (setq paradox-execute-asynchronously t
        paradox-column-width-package 32)
  
  (load *paradox-github-token-file* :noerror :nomessage)
  (paradox-enable))

;;; File handling

;; Keep auto-save and backup files out of the way
(setq backup-directory-alist `((".*" . ,(locate-user-emacs-file ".backup")))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(provide 'init)
;;; init.el ends here
