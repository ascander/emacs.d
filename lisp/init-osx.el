;;; init-osx.el --- OS X configuration settings       -*- lexical-binding: t; -*-

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

;; This file contains specific settings for OS X.

;;; Code:
(when *is-a-mac*
  ;; Modifier keys
  ;;
  ;; These settings are used in conjunction with remapping the modifier keys in
  ;; 'System Preferences' for OS X as follows:
  ;;
  ;;   Caps Lock     => Command
  ;;   Control       => Command
  ;;   Option        => Option
  ;;   Command       => Control
  ;;   Function (fn) => Function (fn)
  ;;
  ;; The combined effect is optimized for Emacs usage, at the expense of some
  ;; unlearning of OS X default keybindings. We get:
  ;;
  ;;   Command   => Control
  ;;   Option    => Meta
  ;;   Control   => Super
  ;;   Caps Lock => Super
  ;;
  ;; Note that 'Hyper' is not used. It's important to have limits.
  (setq mac-command-modifier 'control
        mac-option-modifier 'meta
        mac-control-modifier 'super
        mac-function-modifier 'none)
  ;; Disable passing commands to the system if using Emacs mac port. This
  ;; prevents 'Cmd-h' from hiding the app, instead of sending 'C-h'.
  (if (boundp 'mac-carbon-version-string)
    (validate-setq mac-pass-command-to-system nil))

  (use-package exec-path-from-shell       ; Fix $PATH on GUI Emacs
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

  (use-package osx-trash                  ; OS X Trash support
    :if *is-a-mac*
    :ensure t
    :init (osx-trash-setup)))

;; Set modifier Keys
(when *is-a-mac*
)          ; let OS X use this



(provide 'init-osx)
;;; init-osx.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; coding: utf-8
;; End:
