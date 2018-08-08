;;; init-ivy.el --- Command completion framework and settings  -*- lexical-binding: t; -*-

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

;; This file contains Ivy and related settings.

;;; Code:

(use-package ivy                        ; Minibuffer completion framework
  :pin melpa
  :delight ivy-mode
  :bind (("C-c C-r" . ivy-resume)
         ("s-j"     . ivy-switch-buffer)
         ("C-x B"   . ivy-switch-buffer-other-window))
  :config
  (validate-setq ivy-use-virtual-buffers t
                 ivy-count-format ""
                 ivy-initial-inputs-alist nil)
  (ivy-mode 1))

(use-package counsel                    ; Ivy-powered commands
  :pin melpa
  :after ivy
  :delight counsel-mode
  :bind (([remap execute-extended-command] . counsel-M-x)
         ([remap find-file]                . counsel-find-file)
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
  (use-package smex
    :config (smex-initialize))

  ;; Settings for a counsel powered `org-goto' command, taken from:
  ;; https://github.com/abo-abo/swiper/pull/1005
  (setq counsel-org-goto-display-style 'path
        counsel-org-goto-separator " ➨ "
        counsel-org-goto-face-style 'org)

  (counsel-mode 1))

(use-package swiper                     ; An Ivy-powered alternative to isearch
  :pin melpa
  :after ivy
  :bind (([remap isearch-forward] . swiper))
  :config
  ;; swipe for symbol at point
  (defun ad|swiper-at-point (sym)
    "Use `swiper' to search for the symbol at point."
    (interactive (list (thing-at-point 'symbol)))
    (swiper sym))

  (bind-key "M-s-/" #'ad|swiper-at-point ivy-mode-map))

(use-package ivy-xref                   ; Ivy interface to xref
  :init (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package ivy-rich                   ; More friendly interface for buffer switching
  :after ivy
  :config
  ;; Align virtual buffers, and abbreviate paths
  (setq ivy-virtual-abbreviate 'full
        ivy-rich-path-style 'abbrev
        ivy-rich-switch-buffer-align-virtual-buffer t)

  (ivy-set-display-transformer 'ivy-switch-buffer
                               'ivy-rich-switch-buffer-transformer)
  (ivy-rich-mode 1))

(provide 'init-ivy)
;;; init-ivy.el ends here
