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

;; Constants
(defconst *is-a-mac* (eq system-type 'darwin) "Are we on a mac?")

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
(setq-default use-package-always-ensure t)

(eval-when-compile
  (require 'use-package))

;;; OS X settings

(when *is-a-mac*
  ;; Modifier keys
  (setq mac-command-modifier 'meta      ; Command is Meta
        mac-option-modifier 'super      ; Alt/Option is Super
        mac-function-modifier 'none)    ; Reserve Function for OS X

  ;; Reuse existing frame for opening new files
  (setq ns-pop-up-frames nil))

(use-package osx-trash
  :if *is-a-mac*
  :config
  (osx-trash-setup))

;;; Customization and packages

(use-package cus-edit                   ; The Customization UI
  :ensure nil
  :config
  (setq custom-file (locate-user-emacs-file "custom.el"))
  (load custom-file 'no-error 'no-message))

;;; Basic UI settings

;; Disable tool-bar, scroll-bar, and menu-bar. The menu bar cannot be
;; removed on OS X, so only remove it if you're not on a Mac.
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (and (not *is-a-mac*) (fboundp 'menu-bar-mode)) (menu-bar-mode -1))

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
 require-final-newline t             ; Require a newline at file end
 show-trailing-whitespace nil        ; Don't display trailing whitespaces by default
 split-height-threshold nil          ; Disable vertical window splitting
 split-width-threshold nil           ; Disable horizontal window splitting
 uniquify-buffer-name-style 'forward ; Uniquify buffer names correctly
 window-combination-resize t         ; Resize windows proportionally
 frame-resize-pixelwise t            ; Resize frames by pixel (don't snap to char)
 history-length 1000                 ; Store more history
 use-dialog-box nil)                 ; Don't use dialogues for mouse imput
 
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
  :init
  (add-hook 'after-init-hook #'dimmer-mode))

(use-package stripe-buffer              ; Striped backgorund in `dired'
  :init
  (add-hook 'dired-mode-hook #'stripe-listify-buffer))

;;; Package manager and init development

(use-package paradox                    ; Modernizing Emacs' package menu
  :defer t
  :init
  (defconst *paradox-github-token-file*
    (locate-user-emacs-file "paradox-token.el")
    "Location of the `paradox-github-token' setting for Paradox.")
  :config
  ;; Basic settings
  (setq paradox-execute-asynchronously t
        paradox-spinner-type 'progress-bar
        paradox-column-width-package 32)
  
  (load *paradox-github-token-file* :noerror :nomessage)
  (paradox-enable))

;;; File handling

;; Keep auto-save and backup files out of the way
(setq backup-directory-alist `((".*" . ,(locate-user-emacs-file ".backup")))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Use UTF-8 wherever possible
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

(use-package ffap                       ; Find files at point
  :defer t
  ;; Please stop pinging random hosts! See
  ;; https://github.com/technomancy/emacs-starter-kit/issues/39
  :config (setq ffap-machine-p-known 'reject))

(use-package dired                      ; Edit directories
  :ensure nil
  :defer t
  :init
  (when-let (gls (and *is-a-mac* (executable-find "gls")))
    (setq insert-directory-program gls))
  :config
  ;; Basic dired settings
  (setq dired-auto-revert-buffer t
        dired-listing-switches "-alh"
        dired-recursive-copies 'always
        dired-dwim-target t)

  ;; Use more 'ls' switches if we have them available
  (when (or (memq system-type '(gnu gnu/linux))
            (string= (file-name-nondirectory insert-directory-program) "gls"))
    (setq dired-listing-switches
          (concat dired-listing-switches " --group-directories-first -v"))))

(use-package dired-x                    ; Additional tools for 'dired'
  :ensure nil
  :after dired
  :init
  (add-hook 'dired-mode-hook #'dired-omit-mode)
  :config
  ;; Don't tell me when you're omitting files, and additionally hide some other
  ;; uninteresting files in Dired.
  (setq dired-omit-verbose nil
        dired-omit-files
        (concat dired-omit-files "\\|^.DS_STORE$\\|^.projectile$")))

(use-package ignoramus                  ; Ignore uninteresting files everywhere
  :config
  ;; Ignore some additional files and directories
  (dolist (name '("company-statistics-cache.el"
                  ".ensime"
                  ".ensime_cache"))
    (add-to-list 'ignoramus-file-basename-exact-names name))

  (ignoramus-setup))

(use-package recentf                    ; Save recently visited files
  :init
  (setq recentf-max-saved-items 100     ; Save more recent items
        recentf-max-menu-items 15       ; In the menu, too
        recentf-auto-cleanup 300        ; Cleanup after 300 idle seconds

        ;; Exclude some boring files
        recentf-exclude (list "/\\.git/.*\\'" ; Git contents
                              "/elpa/.*\\'"   ; Package files
                              #'ignoramus-boring-p))
  :config (recentf-mode t))

(use-package autorevert
  :delight auto-revert-mode
  :init
  ;; Basic settings
  (setq auto-revert-verbose nil                ; Autorevert quietly
        global-auto-revert-non-file-buffers t) ; Dired, too

  ;; File notifications are not used on OS X
  (when *is-a-mac*
    (setq auto-revert-use-notify nil))
  :config (global-auto-revert-mode t))

;;; Buffer, frame and window settings

(use-package ibuffer                       ; A better buffer list
  :bind (([remap list-buffers] . ibuffer)) ; C-x C-b
  :config
  (setq ibuffer-formats
        '((mark modified read-only vc-status-mini " "
                (name 18 18 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                (vc-status 16 16 :left)
                " "
                filename-and-process)
          (mark modified read-only " "
                (name 18 18 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " " filename-and-process)
          (mark " " (name 16 -1) " " filename))))

(use-package ibuffer-vc                 ; Group buffers by VC project
  :commands ibuffer-vc-set-filter-groups-by-vc-root
  :hook (ibuffer . (lambda ()
                     (ibuffer-vc-set-filter-groups-by-vc-root)
                     (unless (eq ibuffer-sorting-mode 'alphabetic)
                       (ibuffer-do-sort-by-alphabetic)))))

(use-package ace-window                 ; Fast window switching
  :bind (("M-o" . ace-window))
  :config
  ;; Set face for `aw-leading-char-face'
  (set-face-attribute 'aw-leading-char-face nil
                      :foreground "deep sky blue"
                      :height 2.0)
  ;; Settings
  (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l)
        aw-dispatch-always t
        aw-dispatch-alist
        '((?x aw-delete-window     "Ace - Delete Window")
          (?c aw-swap-window       "Ace - Swap Window")
          (?n aw-flip-window       "Ace - Flip Window")
          (?v aw-split-window-vert "Ace - Split Vert Window")
          (?h aw-split-window-horz "Ace - Split Horiz Window")
          (?m delete-other-windows "Ace - Maximize Window")
          (?g delete-other-windows)
          (?b balance-windows)
          (?u winner-undo)
          (?r winner-redo))))

(use-package focus-autosave-mode        ; Save buffers when Emacs loses focus
  :ensure t
  :delight focus-autosave-mode
  :config
  (focus-autosave-mode))

(use-package winner                     ; Undo/redo window configuration changes
  :config (winner-mode t))

(use-package windmove                   ; Navigate windows using arrow keys
  :config
  ;; Wrap around the edge of the frame, and user Super instead of Shift, because
  ;; the Shift key causes conflicts in `org-mode'.
  (setq windmove-wrap-around t)
  (windmove-default-keybindings 'super))

;;; Project management

(use-package projectile                 ; Project management for Emacs
  :defer 2
  :delight projectile-mode
  :config
  ;; Basic settings
  (setq projectile-completion-system 'ivy
        projectile-find-dir-includes-top-level t
        projectile-switch-project-action #'projectile-dired)
  
  ;; Remove dead projects when Emacs is idle
  (run-with-idle-timer 10 nil #'projectile-cleanup-known-projects)

  (projectile-global-mode))

;;; Version control

(use-package what-the-commit            ; Random commit message
  :bind (("C-c i w" . what-the-commit-insert)
         ("C-c g w" . what-the-commit)))

(use-package magit                      ; The one and only Git front end 
  :ensure t
  :bind (("C-c g c" . magit-clone)
         ("C-c g s" . magit-status)
         ("C-c g b" . magit-blame)
         ("C-c g p" . magit-pull))
  :config
  ;; Basic settings
  (setq magit-save-repository-buffers 'dontask
        magit-refs-show-commit-count 'all
        magit-branch-prefer-remote-upstream '("master")
        magit-branch-adjust-remote-upstream-alist '(("origin/master" "master"))
        magit-revision-show-gravatars nil
        magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)

  ;; Show refined hunks during diffs
  (set-default 'magit-diff-refine-hunk t)

  ;; Set Magit's repo dirs for `magit-status' from Projectile's known projects.
  ;; Initialize the `magit-repository-directories' immediately after Projectile
  ;; was loaded, and update it every time we switched projects, because the new
  ;; project might have been unknown before
  (defun ad|magit-set-repo-dirs-from-projectile ()
    "Set `magit-repo-dirs' from known Projectile projects."
    (let ((project-dirs (bound-and-true-p projectile-known-projects)))
      ;; Remove trailing slashes from project directories, because Magit adds
      ;; trailing slashes again, which breaks the presentation in the Magit
      ;; prompt.
      (setq magit-repository-directories
                     (mapcar #'directory-file-name project-dirs))))
  (with-eval-after-load 'projectile
    (ad|magit-set-repo-dirs-from-projectile))

  (add-hook 'projectile-switch-project-hook
            #'ad|magit-set-repo-dirs-from-projectile)

  ;; Refresh `magit-status' after saving a buffer
  (add-hook 'after-save-hook #'magit-after-save-refresh-status))

(use-package git-commit                 ; git commit message mode
  :defer t
  :config
  ;; Don't check style conventions.
  (remove-hook 'git-commit-finish-query-functions
               #'git-commit-check-style-conventions))

(use-package gitconfig-mode             ; Gitconfig editing
  :defer t)

(use-package gitignore-mode             ; '.gitignore' file editing
  :defer t)

(use-package gitattributes-mode         ; '.gitattributes' file editing
  :defer t)

(use-package git-timemachine            ; Go back in Git time
  :bind (("C-c g t" . git-timemachine)))

(provide 'init)
;;; init.el ends here
