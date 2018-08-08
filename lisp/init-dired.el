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
  :ensure nil
  :defer t
  :init
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
  ("=" diredp-ediff)         ;; smart diff
  ("?" dired-summary)
  ("$" diredp-hide-subdir-nomove)
  ("A" dired-do-find-regexp)
  ("C" dired-do-copy)        ;; Copy all marked files
  ("D" dired-do-delete)
  ("E" dired-mark-extension)
  ("e" dired-ediff-files)
  ("F" dired-do-find-marked-files)
  ("G" dired-do-chgrp)
  ("g" revert-buffer)        ;; read all directories again (refresh)
  ("i" dired-maybe-insert-subdir)
  ("l" dired-do-redisplay)   ;; relist the marked or singel directory
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
  ("v" dired-view-file)      ;; q to exit, s to search, = gets line #
  ("w" dired-kill-subdir)
  ("Y" dired-do-relsymlink)
  ("z" diredp-compress-this-file)
  ("Z" dired-do-compress)
  ("q" nil)
  ("." nil :color blue))
  :config
  (validate-setq
   dired-auto-revert-buffer t		; revert on revisit
   dired-listing-switches "-alh"	; use sensible switches
   dired-recursive-copies 'always	; inhibit prompts for simple recursive operations
   dired-dwim-target t)			; auto-copy to other dired split window

  (bind-key "." #'hydra-dired/body dired-mode-map)

  (add-hook 'dired-mode-hook (lambda () (hl-line-mode -1)))
  
  ;; If on a GNU system, or we have GNU 'ls' available, add some more
  ;; switches: '--group-directories-first' lists directories before
  ;; files, and '-v' sorts numbers in file names correctly.
  (when (or (memq system-type '(gnu gnu/linux))
	    (string= (file-name-nondirectory insert-directory-program) "gls"))
    (validate-setq
     dired-listing-switches (concat dired-listing-switches " --group-directories-first -v"))))

(use-package dired-x			; additional tools for dired
  :ensure nil
  :defer t
  :after dired
  :init (add-hook 'dired-mode-hook #'dired-omit-mode)
  :config
  (validate-setq dired-omit-verbose nil) ; shut up, dired
  (when *is-a-mac*
    ;; OS X 'tar' is close enough to GNU 'tar'.
    (validate-setq dired-guess-shell-gnutar "tar"))

  ;; ;; Diminish `dired-omit-mode'. This can't be done in the usual way
  ;; ;; with ':diminish' because it is explicitly set in
  ;; ;; `dired-omit-startup'.
  ;; (add-function :after (symbol-function 'dired-omit-startup)
  ;;   	(lambda () (diminish 'dired-omit-mode))
  ;;   	'((name . dired-omit-mode-diminish)))
  )

(use-package stripe-buffer		; add stripes to a buffer
  :ensure t
  :init (add-hook 'dired-mode-hook #'stripe-listify-buffer))

(provide 'init-dired)
;;; init-dired.el ends here
