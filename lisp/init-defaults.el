;;; init-defaults.el --- Part of my Emacs setup      -*- lexical-binding: t; -*-

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

;; Default settings for Emacs

;;; Code:

;; Disable scroll bar, tool bar, and menu bar when not on OS X. Note that
;; disabling the menu bar is not possible on OS X. Running Emacs in
;; `fullscreen-mode' hides the menu bar, but it cannot be permanently hidden.
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (and (not *is-a-mac*) (fboundp 'menu-bar-mode) (menu-bar-mode -1)))

;; Set some sensible defaults
(setq-default
 blink-cursor-mode -1                       ; No blinking
 ring-bell-function #'ignore                ; No ringing
 inhibit-startup-screen t                   ; No startup screen
 initial-scratch-message ""                 ; No message in the scratch buffer
 auto-window-vscroll nil                    ; Lighten vertical scroll
 cursor-in-non-selected-windows nil         ; Hide the cursor in inactive windows
 delete-by-moving-to-trash t                ; Delete files to trash
 fill-column 80                             ; Set width for modern displays
 help-window-select t                       ; Focus new help windows when opened
 indent-tabs-mode nil                       ; Stop using tabs to indent
 tab-width 4                                ; But set their width properly
 left-margin-width 0                        ; No left margin
 right-margin-width 0                       ; No right margin
 recenter-positions '(12 top bottom)        ; Set re-centering positions
 scroll-conservatively 1000                 ; Never recenter point while scrolling
 mouse-wheel-progressive-speed nil          ; Scroll linearly with trackpad input
 mouse-wheel-scroll-amount '(1)             ; Scroll by one line with trackpad input
 select-enable-clipboard t                  ; Merge system's and Emacs' clipboard
 sentence-end-double-space nil              ; Single space after a sentence end
 show-trailing-whitespace nil               ; Don't display trailing whitespaces by default
 split-height-threshold nil                 ; Disable vertical window splitting
 split-width-threshold nil                  ; Disable horizontal window splitting
 uniquify-buffer-name-style 'forward        ; Uniquify buffer names correctly
 window-combination-resize t)               ; Resize windows proportionally

;; Miscellaneous default settings
(fset 'yes-or-no-p 'y-or-n-p)                      ; Replace yes/no prompts with y/n
(fset 'display-startup-echo-area-message #'ignore) ; No startup message in the echo area
(delete-selection-mode 1)                          ; Replace region when inserting text
(put 'downcase-region 'disabled nil)               ; Enable downcase-region
(put 'upcase-region 'disabled nil)                 ; Enable upcase-region
(global-hl-line-mode)                              ; Highlight the current line
(line-number-mode)                                 ; Display line number in the mode line
(column-number-mode)                               ; Display column number in the mode line

(provide 'init-defaults)
;;; init-defaults.el ends here
