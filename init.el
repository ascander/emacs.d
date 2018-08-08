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

;; -----------------------------------------------------------------------------
;; Init debugging
;; -----------------------------------------------------------------------------

(setq debug-on-error t)
(setq message-log-max 10000)

;; -----------------------------------------------------------------------------
;; Adjust garbage collection thresholds during startup, and thereafter
;; -----------------------------------------------------------------------------

(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'after-init-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; -----------------------------------------------------------------------------
;; Configure package management
;; -----------------------------------------------------------------------------

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
(require 'delight)
(setq-default use-package-always-ensure t)

;; Benchmark init time
(use-package benchmark-init
  :ensure t
  :config
  ;; Disable collection of benchmark data after initialization
  (add-hook 'after-init-hook #'benchmark-init/deactivate))

;; -----------------------------------------------------------------------------
;; Customization, Environment, and OS settings
;; -----------------------------------------------------------------------------

(use-package server                     ; Emacs server
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

;; Time execution of initialization
(add-hook 'after-init-hook (lambda () (message "Time to load init file: %s"
                                               (emacs-init-time))))

(use-package restart-emacs              ; Restart Emacs from inside Emacs
  :defer t)

(use-package esup                       ; Profile Emacs startup from inside Emacs
  :disabled t
  :defer t)

;; -----------------------------------------------------------------------------
;; Initialize specific features
;; -----------------------------------------------------------------------------

;; Add files in my `.emacs.d/lisp' directory
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(use-package init-defaults :ensure nil)
(use-package init-osx :ensure nil)
(use-package init-ui :ensure nil)
(use-package init-modeline :ensure nil)
(use-package init-themes :ensure nil)
(use-package init-fonts :ensure nil)
(use-package init-files :ensure nil)
(use-package init-dired :ensure nil)
(use-package init-projectile :ensure nil)
(use-package init-org :ensure nil)
(use-package init-keys :ensure nil)
(use-package init-editing :ensure nil)
(use-package init-windows :ensure nil)
(use-package init-ivy :ensure nil)
(use-package init-hydra :ensure nil)
(use-package init-git :ensure nil)
(use-package init-prog :ensure nil)
(use-package init-company :ensure nil)
(use-package init-scala :ensure nil)
(use-package init-markdown :ensure nil)

(provide 'init)
;;; init.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; coding: utf-8
;; no-byte-compile: t
;; End:
