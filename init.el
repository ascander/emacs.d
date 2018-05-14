;;; init.el --- Ascander Dost's Emacs configuration -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2018  Ascander Dost

;; Author: Ascander Dost
;; URL: https://github.com/ascander/emacs.d
;; Keywords: convenience

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

(defconst *spell-check-support-enabled* nil)   ; enable with `t'
(defconst *is-a-mac* (eq system-type 'darwin)) ; are we on a mac?

;; --------------------------------------------------------------------------------
;; Init debugging
;; --------------------------------------------------------------------------------

(setq debug-on-error t)
(setq message-log-max 10000)

;; --------------------------------------------------------------------------------
;; Adjust garbage collection thresholds during startup, and thereafter
;; --------------------------------------------------------------------------------

(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'after-init-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; --------------------------------------------------------------------------------
;; Configure package management
;; --------------------------------------------------------------------------------

(require 'package)
(setq-default
 load-prefer-newer t                    ; prefer the newest version of a file
 package-enable-at-startup nil)         ; activate packages after initialization
(setq package-archives
      ;; Package archives, the usual suspects
      '(("gnu"          . "http://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa"        . "https://melpa.org/packages/")
        ("org"          . "https://orgmode.org/elpa/"))
      package-archive-priorities
      '(("melpa-stable" . 10)
        ("gnu"          . 5)
        ("melpa"        . 0)))
(package-initialize)

;; Bootstrap `use-package'
(unless (and (package-installed-p 'use-package)
             (package-installed-p 'delight))
  (package-refresh-contents)
  (package-install 'use-package t)
  (package-install 'delight t))

(eval-when-compile (require 'use-package)) ; Auto-requires `bind-key' also

;; Benchmark init time
(use-package benchmark-init
  :ensure t
  :config
  ;; Disable collection of benchmark data after initialization
  (add-hook 'after-init-hook #'benchmark-init/deactivate))

;; --------------------------------------------------------------------------------
;; Customization, Environment, and OS settings
;; --------------------------------------------------------------------------------

<<<<<<< Updated upstream
(use-package server                     ; Run Emacs in daemon mode
=======
(use-package server                     ; Emacs server
>>>>>>> Stashed changes
  :ensure t
  :init (server-mode)
  :config (unless (server-running-p)
            (server-start)))

(use-package validate                   ; Validate options
  :ensure t)

(use-package paradox                    ; Modern Emacs package menu
  :ensure t
  :defer t
  :config
  (validate-setq
   paradox-column-width-package 32
   paradox-column-width-version 18
   paradox-execute-asynchronously t)
  (remove-hook 'paradox-after-execution-functions #'paradox--report-buffer-print))

;; Location of the `custom.el' file.
(defconst *custom-file* (locate-user-emacs-file "custom.el")
  "File used to store settings from Customization UI.")

(use-package cus-edit                   ; Customize interface
  :init (load *custom-file* 'no-error 'no-message)
  :config
  (validate-setq custom-file *custom-file*
                 custom-buffer-done-kill nil
                 custom-buffer-verbose-help nil
                 custom-unlispify-tag-names nil
                 custom-unlispify-menu-entries nil))

;; Time execution of initialization
(add-hook 'after-init-hook (lambda () (message "Time to load init file: %s"
                                               (emacs-init-time))))

;; --------------------------------------------------------------------------------
;; Initialize specific features
;; --------------------------------------------------------------------------------

;; Add files in my `.emacs.d/lisp' directory
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(use-package init-defaults)
(use-package init-osx)
(use-package init-ui)
(use-package init-modeline)
(use-package init-themes)
(use-package init-fonts)
(use-package init-files)
(use-package init-dired)
(use-package init-projectile)
(use-package init-org)
(use-package init-keys)
(use-package init-editing)
(use-package init-windows)
(use-package init-ivy)
(use-package init-hydra)
(use-package init-git)
(use-package init-prog)
(use-package init-company)
(use-package init-scala)

(provide 'init)
;;; init.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; coding: utf-8
;; no-byte-compile: t
;; End:
