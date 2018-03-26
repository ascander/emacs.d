;;; init-windows.el --- Frame and Window configuration  -*- lexical-binding: t; -*-

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

;; This file contains settings for frames and windows.

;;; Code:

(use-package frame                      ; frames
  :bind (("M-ƒ" . toggle-frame-fullscreen))
  :init (progn
          ;; Kill `suspend-frame'
          (global-set-key (kbd "C-z") nil)
          (global-set-key (kbd "C-x C-z") nil))
  :config
  (add-to-list 'initial-frame-alist '(fullscreen . maximized)))

(use-package ace-window                 ; fast window switching
  :ensure t
  :defer t
  :bind (("M-o" . ace-window))
  :init
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-background nil))

(use-package winner                     ; undo/redo window configurations
  :ensure t
  :init (winner-mode))

(use-package windmove                   ; navigate between windows with Shift + arrow keys
  :ensure t
  :config
  (when (fboundp 'windmove-default-keybindings)
    (windmove-default-keybindings)))

;; ;;----------------------------------------------------------------------------
;; ;; When splitting window, show (other-buffer) in the new window
;; ;;----------------------------------------------------------------------------
;; (defun split-window-func-with-other-buffer (split-function)
;;   (lambda (&optional arg)
;;     "Split this window and switch to the new window unless ARG is provided."
;;     (interactive "P")
;;     (funcall split-function)
;;     (let ((target-window (next-window)))
;;       (set-window-buffer target-window (other-buffer))
;;       (unless arg
;;         (select-window target-window)))))

;; (global-set-key (kbd "C-x 2") (split-window-func-with-other-buffer 'split-window-vertically))
;; (global-set-key (kbd "C-x 3") (split-window-func-with-other-buffer 'split-window-horizontally))

;; (defun sanityinc/toggle-delete-other-windows ()
;;   "Delete other windows in frame if any, or restore previous window config."
;;   (interactive)
;;   (if (and winner-mode
;;            (equal (selected-window) (next-window)))
;;       (winner-undo)
;;     (delete-other-windows)))

;; (global-set-key (kbd "C-x 1") 'sanityinc/toggle-delete-other-windows)

;; ;;----------------------------------------------------------------------------
;; ;; Rearrange split windows
;; ;;----------------------------------------------------------------------------
;; (defun split-window-horizontally-instead ()
;;   (interactive)
;;   (save-excursion
;;     (delete-other-windows)
;;     (funcall (split-window-func-with-other-buffer 'split-window-horizontally))))

;; (defun split-window-vertically-instead ()
;;   (interactive)
;;   (save-excursion
;;     (delete-other-windows)
;;     (funcall (split-window-func-with-other-buffer 'split-window-vertically))))

;; (global-set-key (kbd "C-x |") 'split-window-horizontally-instead)
;; (global-set-key (kbd "C-x _") 'split-window-vertically-instead)

;; 
;; ;; Borrowed from http://postmomentum.ch/blog/201304/blog-on-emacs
;; (defun sanityinc/split-window()
;;   "Split the window to see the most recent buffer in the other window.
;; Call a second time to restore the original window configuration."
;;   (interactive)
;;   (if (eq last-command 'sanityinc/split-window)
;;       (progn
;;         (jump-to-register :sanityinc/split-window)
;;         (setq this-command 'sanityinc/unsplit-window))
;;     (window-configuration-to-register :sanityinc/split-window)
;;     (switch-to-buffer-other-window nil)))

;; (global-set-key (kbd "<f7>") 'sanityinc/split-window)


;; 
;; (defun sanityinc/toggle-current-window-dedication ()
;;   "Toggle whether the current window is dedicated to its current buffer."
;;   (interactive)
;;   (let* ((window (selected-window))
;;          (was-dedicated (window-dedicated-p window)))
;;     (set-window-dedicated-p window (not was-dedicated))
;;     (message "Window %sdedicated to %s"
;;              (if was-dedicated "no longer " "")
;;              (buffer-name))))

;; (global-set-key (kbd "C-c <down>") 'sanityinc/toggle-current-window-dedication)


;; 
;; (unless (memq window-system '(nt w32))
;;   (windmove-default-keybindings 'control))

(provide 'init-windows)
;;; init-windows.el ends here
