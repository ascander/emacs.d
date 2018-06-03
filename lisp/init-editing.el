;;; init-editing.el --- Basic file editing settings  -*- lexical-binding: t; -*-

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

;; This file contains settings for basic file editing.

;;; Code:

(use-package smartparens                ; Dealing with pairs in Emacs
  :ensure t
  :delight smartparens-mode " ⓟ"
  :init
  (smartparens-global-mode t)
  (show-smartparens-global-mode t)
  :config
  (require 'smartparens-config)
  (sp-use-smartparens-bindings)

  ;; Free keybindings for navigation/deletion
  (bind-key "C-<left>" nil smartparens-mode-map)
  (bind-key "C-<right>" nil smartparens-mode-map)
  (bind-key "M-<backspace>" nil smartparens-mode-map)
  
  ;; Helpful wrapping commands
  (sp-pair "(" ")"   :wrap "C-(")
  (sp-pair "[" "]"   :wrap "M-s-[")     ;  "C-[" sends ESC
  (sp-pair "{" "}"   :wrap "C-{")
  (sp-pair "\"" "\"" :wrap "C-\""))

(use-package darkroom                   ; Distraction free editing
  :ensure t
  :delight darkroom-tentative-mode
  :bind (("C-c w D" . darkroom-tentative-mode)))

;; (use-package smartparens
;;   :ensure t
;;   :bind (("C-c k" . ascii-smartparens/body)
;;          :map smartparens-strict-mode-map
;;          ("M-q" . sp-indent-defun))
;;   :init
;;   (defhydra ascii-smartparens (:hint nil)
;;     "
;; Sexps (quit with _q_)

;; ^Nav^            ^Barf/Slurp^                 ^Depth^
;; ^---^------------^----------^-----------------^-----^-----------------
;; _f_: forward     _→_:          slurp forward   _R_: splice
;; _b_: backward    _←_:          barf forward    _r_: raise
;; _u_: backward ↑  _C-<right>_:  slurp backward  _↑_: raise backward
;; _d_: forward ↓   _C-<left>_:   barf backward   _↓_: raise forward
;; _p_: backward ↓
;; _n_: forward ↑

;; ^Kill^           ^Misc^                       ^Wrap^
;; ^----^-----------^----^-----------------------^----^------------------
;; _w_: copy        _j_: join                    _(_: wrap with ( )
;; _k_: kill        _s_: split                   _{_: wrap with { }
;; ^^               _t_: transpose               _'_: wrap with ' '
;; ^^               _c_: convolute               _\"_: wrap with \" \"
;; ^^               _i_: indent defun"
;;     ("q" nil)
;;     ;; Wrapping
;;     ("(" (lambda (_) (interactive "P") (sp-wrap-with-pair "(")))
;;     ("{" (lambda (_) (interactive "P") (sp-wrap-with-pair "{")))
;;     ("'" (lambda (_) (interactive "P") (sp-wrap-with-pair "'")))
;;     ("\"" (lambda (_) (interactive "P") (sp-wrap-with-pair "\"")))
;;     ;; Navigation
;;     ("f" sp-forward-sexp )
;;     ("b" sp-backward-sexp)
;;     ("u" sp-backward-up-sexp)
;;     ("d" sp-down-sexp)
;;     ("p" sp-backward-down-sexp)
;;     ("n" sp-up-sexp)
;;     ;; Kill/copy
;;     ("w" sp-copy-sexp)
;;     ("k" sp-kill-sexp)
;;     ;; Misc
;;     ("t" sp-transpose-sexp)
;;     ("j" sp-join-sexp)
;;     ("s" sp-split-sexp)
;;     ("c" sp-convolute-sexp)
;;     ("i" sp-indent-defun)
;;     ;; Depth changing
;;     ("R" sp-splice-sexp)
;;     ("r" sp-splice-sexp-killing-around)
;;     ("<up>" sp-splice-sexp-killing-backward)
;;     ("<down>" sp-splice-sexp-killing-forward)
;;     ;; Barfing/slurping
;;     ("<right>" sp-forward-slurp-sexp)
;;     ("<left>" sp-forward-barf-sexp)
;;     ("C-<left>" sp-backward-barf-sexp)
;;     ("C-<right>" sp-backward-slurp-sexp))

;;   (smartparens-global-mode)
;;   (show-smartparens-global-mode)

;;   (dolist (hook '(inferior-emacs-lisp-mode-hook
;;                   emacs-lisp-mode-hook))
;;     (add-hook hook #'smartparens-strict-mode))
;;   :config
;;   (require 'smartparens-config)

;;   (validate-setq sp-autoskip-closing-pair 'always
;;                  ;; Don't kill entire symbol on C-k
;;                  sp-hybrid-kill-entire-symbol nil)
;;   :delight smartparens-mode " ⓟ")

;; Prettify symbols when you can
(when (fboundp 'global-prettify-symbols-mode)
  (add-hook 'after-init-hook 'global-prettify-symbols-mode))

;; Newline behavior
;; (defun ad|newline-at-end-of-line ()
;;   "Move to the end of the line, enter a newline, and reindent."
;;   (interactive)
;;   (move-end-of-line nil)
;;   (newline-and-indent))

;; (global-set-key (kbd "M-<return>") 'ad|newline-at-end-of-line)

(use-package autorevert			; auto-revert buffers of changed files
  :ensure t
  :init (global-auto-revert-mode)
  :config
  (validate-setq auto-revert-verbose nil
		 global-auto-revert-non-file-buffers t)
  (when *is-a-mac*
    (validate-setq auto-revert-use-notify nil))
  :diminish auto-revert-mode)

;; Allow narrowing (disabled by default)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; Only display line numbers when they matter; namely when navigating to a
;; specific one via `goto-line'. Note the use of `linum-mode' instead of the
;; more performant `nlinum-mode'; this is because the using the latter results
;; in occasional missing line numbers.
(defun ad|goto-line-with-numbers ()
  "Show line numbers while navigating to a specific one."
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))
(global-set-key [remap goto-line] 'ad|goto-line-with-numbers)

(provide 'init-editing)
;;; init-editing.el ends here
