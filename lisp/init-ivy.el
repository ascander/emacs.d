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

(use-package ivy                        ; minibuffer completion framework
  :ensure t
  :init (ivy-mode 1)
  :bind
  (:map ivy-mode-map
        ("C-'" . ivy-avy))
  :config
  (validate-setq ivy-use-virtual-buffers t
                 ivy-count-format ""
                 ivy-initial-inputs-alist nil)
  :diminish ivy-mode)

(use-package counsel                    ; Ivy-powered commands
  :ensure t
  :init (counsel-mode 1)
  :bind (([remap execute-extended-command] . counsel-M-x)
         ([remap find-file]                . counsel-find-file)
         ([remap describe-function]        . counsel-describe-function)
         ([remap describe-variable]        . counsel-describe-variable)
         ([remap info-lookup-symbol]       . counsel-info-lookup-symbol)
         ([remap completion-at-point]      . counsel-company)
         ("C-c f L"                        . counsel-load-library)
         ("C-c f r"                        . counsel-recentf)
         ("C-c i 8"                        . counsel-unicode-char)
         ("C-c r g"                        . counsel-rg)
         ("C-c j t"                        . counsel-imenu)
         ("C-c g L"                        . counsel-git-log))
  :diminish counsel-mode)

(use-package swiper                     ; isearch with overview
  :ensure t
  :defer t
  :bind (([remap isearch-forward] . swiper)))

(provide 'init-ivy)
;;; init-ivy.el ends here

;; (when (maybe-require-package 'ivy-historian)
;;   (add-hook 'after-init-hook (lambda () (ivy-historian-mode t))))

;; (when (maybe-require-package 'swiper)
;;   (after-load 'ivy
;;     (defun sanityinc/swiper-at-point (sym)
;;       "Use `swiper' to search for the symbol at point."
;;       (interactive (list (thing-at-point 'symbol)))
;;       (swiper sym))

;;     (define-key ivy-mode-map (kbd "M-s /") 'sanityinc/swiper-at-point)))


;; (when (maybe-require-package 'ivy-xref)
;;   (setq xref-show-xrefs-function 'ivy-xref-show-xrefs))
