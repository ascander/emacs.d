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
(setq load-prefer-newer t            ; prefer the newest version of a file
      package-enable-at-startup nil) ; explicitly initialize packages

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu"   . "http://elpa.gnu.org/packages/")
        ("org"   . "https://orgmode.org/elpa/")))
(package-initialize)

;; Bootstrap `use-package'
;; TODO: this should also check/install `delight'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package t))
(setq-default use-package-always-ensure t)

(eval-when-compile
  (require 'use-package))
(require 'delight)
(require 'bind-key)

;;; OS X settings

(when *is-a-mac*
  ;; Modifier keys
  (setq mac-command-modifier 'meta      ; Command is Meta
        mac-option-modifier 'super      ; Alt/Option is Super
        mac-function-modifier 'none)    ; Reserve Function for OS X

  ;; Reuse existing frame for opening new files
  (setq ns-pop-up-frames nil))

(use-package exec-path-from-shell       ; Fix $PATH on GUI Emacs
  :disabled t
  :when *is-a-mac*
  :ensure t
  :config
  (progn
    (validate-setq exec-path-from-shell-check-startup-files nil
                   exec-path-from-shell-variables
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
    (setq user-email-address (getenv "EMAIL")
          user-full-name (getenv "FULLNAME"))

    ;; Re-initialize the `info-directory-list' from $INFOPATH. Since
    ;; `package.el' already initializes info, we need to explicitly
    ;; add the $INFOPATH directories to `info-directory-list'. Reverse
    ;; the list of info paths to prepend them in proper order.
    (with-eval-after-load 'info
      (dolist (dir (nreverse (parse-colon-path (getenv "INFOPATH"))))
        (when dir
          (add-to-list 'Info-directory-list dir))))))

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

;; Default font settings
(defvar default-font-size-pt 12
  "Default font size, in points.")

(set-face-attribute 'default nil
                    :family "Iosevka Type"
                    :height 120
                    :weight 'regular)

(set-face-attribute 'variable-pitch nil
                    :family "Fira Sans"
                    :height 140
                    :weight 'regular)

;; Global font resizing, taken from https://github.com/kaushalmodi/.emacs.d
(defun ad|global-font-size-adj (scale &optional absolute)
  "Adjust the font sizes globally: in all the buffers, mode line, echo area, etc.

The built-in `text-scale-adjust' function does an excellent job
of font resizing, but it does not change the font sizes of text
outside the current buffer; for example, in the mode line.

M-<SCALE> COMMAND increases font size by SCALE points if SCALE is +ve,
                  decreases font size by SCALE points if SCALE is -ve
                  resets    font size if SCALE is 0.

If ABSOLUTE is non-nil, text scale is applied relative to the
default font size `default-font-size-pt'. Else, the text scale is
applied relative to the current font size."
  (interactive "p")
  (if (= scale 0)
      (setq font-size-pt default-font-size-pt)
    (if (bound-and-true-p absolute)
        (setq font-size-pt (+ default-font-size-pt scale))
      (setq font-size-pt (+ font-size-pt scale))))
  ;; The internal font size value is 10x the font size in points unit. So a 10pt
  ;; font size is equal to 100 in internal font size value.
  (set-face-attribute 'default nil :height (* font-size-pt 10)))

(defun ad|global-font-size-incr () (interactive) (ad|global-font-size-adj +1))
(defun ad|global-font-size-decr () (interactive) (ad|global-font-size-adj -1))
(defun ad|global-font-size-reset () (interactive) (ad|global-font-size-adj 0))

;; Initialize font-size-pt var to the default value
(unless (boundp 'font-size-pt)
  (ad|global-font-size-reset))

;; Font resizing hydra, for convenience
(defhydra hydra-font-resize (nil
                             "C-c"
                             :bind (lambda (key cmd) (bind-key key cmd))
                             :color red
                             :hint nil)
  "
Font size:  _-_ decrease  _=_ increase  _0_ reset  _q_uit
"
  ;; Hydra entry bindings
  ("C--" ad|global-font-size-decr)
  ("C-=" ad|global-font-size-incr)
  ("C-0" ad|global-font-size-reset :color blue)
  ;; Hydra-internal bindings, below work only when the hydra is active!
  ("-"   ad|global-font-size-decr :bind nil)
  ("="   ad|global-font-size-incr :bind nil)
  ("+"   ad|global-font-size-incr :bind nil)
  ("0"   ad|global-font-size-reset :bind nil)
  ("q"   nil :color blue))

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
  :defer t
  :init
  ;; NOTE: the face for `stripe-hl-line' is customized via the customization
  ;; interface. It would be good to figure out how to set this here.
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
  :bind (:map dired-mode-map
              ("." . #'hydra-dired/body))
  :init
  (when-let (gls (and *is-a-mac* (executable-find "gls")))
    (setq insert-directory-program gls))

  ;; Hydra from https://github.com/abo-abo/hydra/wiki/Dired
  (defhydra hydra-dired (:hint nil :color pink)
    "
_+_ mkdir          _v_iew           _m_ark             _(_ details        _i_nsert-subdir    wdired
_C_opy             _O_ view other   _U_nmark all       _)_ omit-mode      _$_ hide-subdir    C-x C-q : edit
_D_elete           _o_pen other     _u_nmark           _l_ redisplay      _w_ kill-subdir    C-c C-c : commit
_R_ename           _M_ chmod        _t_oggle           _g_ revert buf     _e_ ediff          C-c ESC : abort
_Y_ rel symlink    _G_ chgrp        _E_xtension mark   _s_ort             _=_ pdiff
_S_ymlink          ^ ^              _F_ind marked      _._ toggle hydra   \\ flyspell
_r_sync            ^ ^              ^ ^                ^ ^                _?_ summary
_z_ compress-file  _A_ find regexp
_Z_ compress       _Q_ repl regexp

T - tag prefix
"
    ("\\" dired-do-ispell)
    ("(" dired-hide-details-mode)
    (")" dired-omit-mode)
    ("+" dired-create-directory)
    ("=" diredp-ediff)                  ; smart diff
    ("?" dired-summary)
    ("$" diredp-hide-subdir-nomove)
    ("A" dired-do-find-regexp)
    ("C" dired-do-copy)                 ; copy all marked files
    ("D" dired-do-delete)
    ("E" dired-mark-extension)
    ("e" dired-ediff-files)
    ("F" dired-do-find-marked-files)
    ("G" dired-do-chgrp)
    ("g" revert-buffer)                 ; read all directories again (refresh)
    ("i" dired-maybe-insert-subdir)
    ("l" dired-do-redisplay)            ; relist the marked or single directory
    ("M" dired-do-chmod)
    ("m" dired-mark)
    ("O" dired-display-file)
    ("o" dired-find-file-other-window)
    ("Q" dired-do-find-regexp-and-replace)
    ("R" dired-do-rename)
    ("r" dired-do-rsynch)
    ("S" dired-do-symlink)
    ("s" dired-sort-toggle-or-edit)
    ("t" dired-toggle-marks)
    ("U" dired-unmark-all-marks)
    ("u" dired-unmark)
    ("v" dired-view-file)               ; q to exit, s to search, = gets line #
    ("w" dired-kill-subdir)
    ("Y" dired-do-relsymlink)
    ("z" diredp-compress-this-file)
    ("Z" dired-do-compress)
    ("q" nil)
    ("." nil :color blue))
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
  :config (recentf-mode 1))

(use-package autorevert
  :delight auto-revert-mode
  :init
  ;; Basic settings
  (setq auto-revert-verbose nil                ; Autorevert quietly
        global-auto-revert-non-file-buffers t) ; Dired, too

  ;; File notifications are not used on OS X
  (when *is-a-mac*
    (setq auto-revert-use-notify nil))
  :config (global-auto-revert-mode 1))

;; View read-only files
(setq view-read-only t)

;;; Buffer, frame and window settings

(use-package ibuffer                       ; A better buffer list
  :bind (([remap list-buffers] . ibuffer)  ; C-x C-b
         :map ibuffer-mode-map
         ("." . #'hydra-ibuffer-main/body))
  :init
  (defhydra hydra-ibuffer-main (:color pink :hint nil)
    "
^Mark^         ^Actions^         ^View^          ^Select^              ^Navigation^
_m_: mark      _D_: delete       _g_: refresh    _q_: quit             _k_:   ↑    _h_
_u_: unmark    _s_: save marked  _S_: sort       _TAB_: toggle         _RET_: visit
_*_: specific  _a_: all actions  _/_: filter     _o_: other window     _j_:   ↓    _l_
_t_: toggle    _._: toggle hydra _H_: help       C-o other win no-select
"
    ("m" ibuffer-mark-forward)
    ("u" ibuffer-unmark-forward)
    ("*" hydra-ibuffer-mark/body :color blue)
    ("t" ibuffer-toggle-marks)

    ("D" ibuffer-do-delete)
    ("s" ibuffer-do-save)
    ("a" hydra-ibuffer-action/body :color blue)

    ("g" ibuffer-update)
    ("S" hydra-ibuffer-sort/body :color blue)
    ("/" hydra-ibuffer-filter/body :color blue)
    ("H" describe-mode :color blue)

    ("h" ibuffer-backward-filter-group)
    ("k" ibuffer-backward-line)
    ("l" ibuffer-forward-filter-group)
    ("j" ibuffer-forward-line)
    ("RET" ibuffer-visit-buffer :color blue)

    ("TAB" ibuffer-toggle-filter-group)

    ("o" ibuffer-visit-buffer-other-window :color blue)
    ("q" quit-window :color blue)
    ("." nil :color blue))

  (defhydra hydra-ibuffer-mark (:color teal :columns 5
                                       :after-exit (hydra-ibuffer-main/body))
    "Mark"
    ("*" ibuffer-unmark-all "unmark all")
    ("M" ibuffer-mark-by-mode "mode")
    ("m" ibuffer-mark-modified-buffers "modified")
    ("u" ibuffer-mark-unsaved-buffers "unsaved")
    ("s" ibuffer-mark-special-buffers "special")
    ("r" ibuffer-mark-read-only-buffers "read-only")
    ("/" ibuffer-mark-dired-buffers "dired")
    ("e" ibuffer-mark-dissociated-buffers "dissociated")
    ("h" ibuffer-mark-help-buffers "help")
    ("z" ibuffer-mark-compressed-file-buffers "compressed")
    ("b" hydra-ibuffer-main/body "back" :color blue))

  (defhydra hydra-ibuffer-action (:color teal :columns 4
                                         :after-exit
                                         (if (eq major-mode 'ibuffer-mode)
                                             (hydra-ibuffer-main/body)))
    "Action"
    ("A" ibuffer-do-view "view")
    ("E" ibuffer-do-eval "eval")
    ("F" ibuffer-do-shell-command-file "shell-command-file")
    ("I" ibuffer-do-query-replace-regexp "query-replace-regexp")
    ("H" ibuffer-do-view-other-frame "view-other-frame")
    ("N" ibuffer-do-shell-command-pipe-replace "shell-cmd-pipe-replace")
    ("M" ibuffer-do-toggle-modified "toggle-modified")
    ("O" ibuffer-do-occur "occur")
    ("P" ibuffer-do-print "print")
    ("Q" ibuffer-do-query-replace "query-replace")
    ("R" ibuffer-do-rename-uniquely "rename-uniquely")
    ("T" ibuffer-do-toggle-read-only "toggle-read-only")
    ("U" ibuffer-do-replace-regexp "replace-regexp")
    ("V" ibuffer-do-revert "revert")
    ("W" ibuffer-do-view-and-eval "view-and-eval")
    ("X" ibuffer-do-shell-command-pipe "shell-command-pipe")
    ("b" nil "back"))

  (defhydra hydra-ibuffer-sort (:color amaranth :columns 3)
    "Sort"
    ("i" ibuffer-invert-sorting "invert")
    ("a" ibuffer-do-sort-by-alphabetic "alphabetic")
    ("v" ibuffer-do-sort-by-recency "recently used")
    ("s" ibuffer-do-sort-by-size "size")
    ("f" ibuffer-do-sort-by-filename/process "filename")
    ("m" ibuffer-do-sort-by-major-mode "mode")
    ("b" hydra-ibuffer-main/body "back" :color blue))

  (defhydra hydra-ibuffer-filter (:color amaranth :columns 4)
    "Filter"
    ("m" ibuffer-filter-by-used-mode "mode")
    ("M" ibuffer-filter-by-derived-mode "derived mode")
    ("n" ibuffer-filter-by-name "name")
    ("c" ibuffer-filter-by-content "content")
    ("e" ibuffer-filter-by-predicate "predicate")
    ("f" ibuffer-filter-by-filename "filename")
    (">" ibuffer-filter-by-size-gt "size")
    ("<" ibuffer-filter-by-size-lt "size")
    ("/" ibuffer-filter-disable "disable")
    ("b" hydra-ibuffer-main/body "back" :color blue))
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
  :init
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
          (?r winner-redo)))

  ;; Add hydras for sizing/frames
  (when (package-installed-p 'hydra)
    (defhydra hydra-window-size (:color red)
      "Window size"
      ("h" shrink-window-horizontally "shrink horizontal")
      ("j" shrink-window "shrink vertical")
      ("k" enlarge-window "enlarge vertical")
      ("l" enlarge-window-horizontally "enlarge horizontal"))
    (defhydra hydra-window-frame (:color red)
      "Frame"
      ("f" make-frame "new frame")
      ("x" delete-frame "delete frame"))

    (add-to-list 'aw-dispatch-alist '(?w hydra-window-size/body) t)
    (add-to-list 'aw-dispatch-alist '(?\; hydra-window-frame/body) t)))

(use-package focus-autosave-mode        ; Save buffers when Emacs loses focus
  :delight focus-autosave-mode
  :config (focus-autosave-mode t))

(use-package winner                     ; Undo/redo window configuration changes
  :config (winner-mode 1))

(use-package windmove                   ; Navigate windows using arrow keys
  :config
  ;; Wrap around the edge of the frame, and user Super instead of Shift, because
  ;; the Shift key causes conflicts in `org-mode'.
  (setq windmove-wrap-around t)
  (windmove-default-keybindings 'super))

;; Quicker buffer cycling commands
(bind-keys ("s-p" . previous-buffer)
           ("s-n" . next-buffer)
           ("s-k" . kill-this-buffer))

;;; Org

(use-package init-org                   ; The almighty Org mode
  :load-path "lisp"
  :ensure nil)

;;; Project management

(use-package projectile                 ; Project management for Emacs
  :defer 2
  :delight projectile-mode
  :bind-keymap ("s-p" . projectile-command-map)
  :config
  ;; Basic settings
  (setq projectile-completion-system 'ivy
        projectile-find-dir-includes-top-level t
        projectile-switch-project-action #'projectile-dired)

  ;; Remove dead projects when Emacs is idle
  (run-with-idle-timer 10 nil #'projectile-cleanup-known-projects)

  (projectile-global-mode t))

;;; Version control

(use-package what-the-commit            ; Random commit message
  :bind (("C-c i w" . what-the-commit-insert)
         ("C-c g w" . what-the-commit)))

(use-package magit                      ; The one and only Git front end
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

;;; Completion

(use-package hydra                      ; Make Emacs bindings that stick around
  :defer t)

(use-package ivy                        ; Generic completion mechanism for Emacs
  :delight ivy-mode
  :demand t
  :bind (("s-j" . ivy-switch-buffer)
         ("s-J" . ivy-switch-buffer-other-window)
         ("s-r" . ivy-resume))
  :config
  ;; Basic settings
  (setq ivy-use-virtual-buffers t
        ivy-initial-inputs-alist nil
        ivy-count-format "")

  (ivy-mode 1))

(use-package ivy-hydra                  ; A useful hydra for the Ivy minibuffer
  :after (ivy hydra)
  :defer t
  :config
  (bind-key "C-o" #'hydra-ivy/body ivy-minibuffer-map))

(use-package ivy-rich
  :after ivy
  :config
  ;; Align virtual buffers, and abbreviate paths
  (setq ivy-virtual-abbreviate 'full
        ivy-rich-path-style 'abbrev
        ivy-rich-switch-buffer-align-virtual-buffer t)

  (ivy-set-display-transformer 'ivy-switch-buffer
                               'ivy-rich-switch-buffer-transformer)
  (ivy-rich-mode 1))

(use-package counsel                    ; Ivy-enhanced versions of commands
  :after ivy
  :demand t
  :delight counsel-mode
  :bind (([remap execute-extended-command] . counsel-M-x)
         ([remap find-file]                . counsel-find-file)
         ([remap describe-face]            . counsel-describe-face)
         ([remap describe-function]        . counsel-describe-function)
         ([remap describe-variable]        . counsel-describe-variable)
         ([remap info-lookup-symbol]       . counsel-info-lookup-symbol)
         ([remap completion-at-point]      . counsel-company)
         ([remap org-goto]                 . counsel-org-goto)
         ("C-c f L"                        . counsel-load-library)
         ("C-c f r"                        . counsel-recentf)
         ("C-c i 8"                        . counsel-unicode-char)
         ("C-c r g"                        . counsel-rg)
         ("C-c j t"                        . counsel-imenu)
         ("C-c g L"                        . counsel-git-log))
  :config
  ;; Use Smex ranking of results automatically
  (use-package smex
    :config (smex-initialize))
  ;; TODO: counsel powered `org-goto' command
  (counsel-mode 1))

(use-package counsel-projectile         ; Counsel integration with Projectile
  :load-path "site-lisp/counsel-projectile"
  :after (counsel projectile)
  :config
  ;; TODO: remap `projectile-ag' to `counsel-projectile-rg'
  (counsel-projectile-mode 1))

(use-package swiper                     ; An Ivy-enhanced alternative to isearch
  :after ivy
  :bind (([remap isearch-forward] . swiper)))

;;; Keys and keybindings

(use-package which-key                  ; Display keybindings based on current command
  :defer 5
  :delight
  :init
  (setq which-key-idle-delay 0.4
        which-key-sort-order 'which-key-prefix-then-key-order
        which-key-replacement-alist
        '(
          ;; Replacements for how all or part of FUNCTION is replaced when
          ;; `which-key' displays:
          ;;
          ;;     KEY → Function
          ;;
          ;; Eg: after "C-c g" display "s → magit-status" as "s → git-status"
          ((nil . "Prefix Command")            . (nil . "prefix"))
          ((nil . "\\`\\?\\?\\'")              . (nil . "λ"))
          ((nil . "projectile-")               . (nil . "pj-"))
          ((nil . "magit-")                    . (nil . "git-"))
          ((nil . "\\`hydra-\\(.+\\)/body\\'") . (nil . "=|\\1"))
          ((nil . "\\`hydra-\\(.+\\)\\'")      . (nil . "=|\\1"))))
  :config
  (which-key-mode 1))

(use-package free-keys                  ; Show free keybindings in the current buffer
  :defer 5)

;;; Basic editing

;; Display line numbers when they matter; namely, when navigating to a specific
;; line via `goto-line'.
(defun ad|goto-line-with-numbers ()
  "Show line numbers while navigating to a specific line."
  (interactive)
  (unwind-protect
      (progn
        (display-line-numbers-mode 1)
        (goto-line (read-number "Goto line: ")))
    (display-line-numbers-mode -1)))
(global-set-key [remap goto-line] #'ad|goto-line-with-numbers)

(use-package writeroom-mode             ; Distraction free editing mode
  :bind (:map writeroom-mode-map
              ("C-M-<" . #'writeroom-decrease-width)
              ("C-M->" . #'writeroom-increase-width)
              ("C-M-=" . #'writeroom-adjust-width)
              ("s-m"   . #'writeroom-toggle-mode-line)))

;; Allow narrowing (disabled by default)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

(use-package smartparens                ; Deal with pairs in Emacs
  :delight
  :config
  (require 'smartparens-config)

  ;; Free bindings for navigation
  (bind-key "C-<left>" nil smartparens-mode-map)
  (bind-key "C-<right>" nil smartparens-mode-map)
  (bind-key "M-<backspace>" nil smartparens-mode-map)

  ;;  Activate smartparens globally
  (smartparens-global-mode 1)
  (show-smartparens-global-mode 1))

;;; Programming Settings

;; Make symbols pretty in programming contexts
(when (fboundp 'global-prettify-symbols-mode)
  (add-hook 'prog-mode-hook #'global-prettify-symbols-mode))

(use-package rainbow-delimiters         ; Highlight delimiters by depth
  :defer t
  :hook ((prog-mode . rainbow-delimiters-mode)
         (text-mode . rainbow-delimiters-mode)))

(use-package rainbow-mode               ; Fontify colors in buffers
  :bind (("C-c t r" . rainbow-mode))
  :delight
  :hook prog-mode
  :init
  ;; Don't highlight color names (just color codes, thanks)
  (setq rainbow-x-colors nil))

(use-package whitespace-cleanup-mode    ; Intelligently clean up whitespace before buffers are saved
  :bind (("C-c x w" . whitespace-cleanup))
  :hook (prog-mode text-mode conf-mode)
  :delight)

(use-package eldoc                      ; Print argument information in the echo area
  :delight eldoc-mode
  :defer t
  :hook (eval-expression-minibuffer-setup . eldoc-mode))

(use-package deadgrep                   ; Fast, beautiful text search
  :bind ("s-g" . deadgrep))

(use-package expand-region              ; Expand the selected region by semantic units
  :bind ("C-=" . er/expand-region))

(use-package dumb-jump                  ; Jump to definition dumbly
  :hook ((prog-mode . dumb-jump-mode))
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g b" . dumb-jump-back)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g l" . dumb-jump-quick-look))
  :init
  (setq dumb-jump-selector 'ivy
        dumb-jump-prefer-searcher 'rg))

(use-package highlight-symbol           ; Automatic and manual symbol highlighting for Emacs
  :bind (("C-c h s" . highlight-symbol)
         ("C-c h n" . highlight-symbol-next)
         ("C-c h p" . highlight-symbol-prev)
         ("C-c h r" . highlight-symbol-query-replace)))

(use-package yasnippet                  ; Snippets
  :commands (yas-reload-all yas-minor-mode)
  :delight yas-minor-mode " |ⓨ"
  :hook ((prog-mode . yas-minor-mode))
  :config (yas-reload-all))

(use-package yasnippet-snippets         ; Official snippets collection
  :after yasnippet)

;;; Markdown support

(use-package markdown-mode              ; Major mode for editing Markdown/GFM files
  :commands (markdown-mode gfm-mode)
  :mode (("\\README\\.md\\'" . gfm-mode)
         ("\\.md\\'"          . markdown-mode)
         ("\\.markdown\\'"    . markdown-mode))
  :init
  ;; Turn off auto-fill for GFM markdown
  (add-hook 'gfm-mode-hook #'turn-off-auto-fill)
  (add-hook 'gfm-mode-hook #'visual-line-mode)
  (setq markdown-command "multimarkdown")
  :config
  ;; Turn off 'M-q' in GFM markdown - it's too easy to do by mistake
  (bind-key "M-q" #'ignore gfm-mode-map))

(use-package markdown-preview-mode      ; Preview markdown
  :after markdown-mode
  :config
  (setq markdown-preview-stylesheets
        (list (concat "https://github.com/dmarcotte/github-markdown-preview/"
                      "blob/master/data/css/github.css"))))

(provide 'init)
;;; init.el ends here
