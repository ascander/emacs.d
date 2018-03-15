;;; init-dired.el --- Directory editor settings      -*- lexical-binding: t; -*-

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

;; This file contains `dired-mode' settings.

;;; Code:

;; Set coreutils 'gls' version of 'ls' when available
(when-let (gls (and *is-a-mac* (executable-find "gls")))
  (validate-setq insert-directory-program gls))

(use-package dired			; edit directories
  :defer t
  :config
  (validate-setq
   dired-auto-revert-buffer t		; revert on revisit
   dired-listing-switches "-alh"	; use sensible switches
   dired-recursive-copies 'always	; inhibit prompts for simple recursive operations
   dired-dwim-target t)			; auto-copy to other dired split window

  ;; If on a GNU system, or we have GNU 'ls' available, add some more
  ;; switches: '--group-directories-first' lists directories before
  ;; files, and '-v' sorts numbers in file names correctly.
  (when (or (memq system-type '(gnu gnu/linux))
	    (string= (file-name-nondirectory insert-directory-program) "gls"))
    (validate-setq
     dired-listing-switches (concat dired-listing-switches " --group-directories-first -v"))))

(use-package dired-x			; additional tools for dired
  :defer t
  :after dired
  :init (add-hook 'dired-mode-hook #'dired-omit-mode)
  :config
  (validate-setq dired-omit-verbose nil) ; shut up, dired
  (when *is-a-mac*
    ;; OS X 'tar' is close enough to GNU 'tar'.
    (validate-setq dired-guess-shell-gnutar "tar"))

  ;; Diminish `dired-omit-mode'. This can't be done in the usual way
  ;; with ':diminish' because it is explicitly set in
  ;; `dired-omit-startup'.
  (add-function :after (symbol-function 'dired-omit-startup)
		(lambda () (diminish 'dired-omit-mode))
		'((name . dired-omit-mode-diminish))))

(use-package stripe-buffer		; add stripes to a buffer
  :ensure t
  :init (add-hook 'dired-mode-hook #'stripe-buffer-mode))

(provide 'init-dired)
;;; init-dired.el ends here
