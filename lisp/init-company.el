;;; init-company.el --- Company code completion settings  -*- lexical-binding: t; -*-

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

;; This file contains settings for `company-mode'.

;;; Code:

(use-package company                    ; Graphical (auto-)completion
  :defer 2
  :delight company-mode
  :config
  (global-company-mode)

  (validate-setq company-dabbrev-ignore-case nil
                 company-dabbrev-code-ignore-case nil
                 company-dabbrev-downcase nil
                 company-idle-delay 0.1
                 company-tooltip-align-annotations t
                 company-tooltip-flip-when-above t
                 ;; Easy navigation to candidates with M-<n>
                 company-show-numbers t)

  ;; Disable TAB in company-mode, freeing it for Yasnippet
  (define-key company-active-map [tab] nil)
  (define-key company-active-map (kbd "TAB") nil)
  :diminish company-mode)

(use-package company-quickhelp          ; show help in tooltip
  :ensure t
  :after company
  :config (company-quickhelp-mode))

(use-package company-statistics         ; sort company candidates by statistics
  :ensure t
  :after company
  :config (company-statistics-mode))

(use-package company-math               ; completion for math symbols
  :ensure t
  :after company
  :config
  ;; Add backends for math characters
  (add-to-list 'company-backends 'company-math-symbols-unicode))

(provide 'init-company)
;;; init-company.el ends here

;; (setq tab-always-indent 'complete)
;; (add-to-list 'completion-styles 'initials t)

;; (when (maybe-require-package 'company)
;;   (add-hook 'after-init-hook 'global-company-mode)
;;   (after-load 'company
;;     (diminish 'company-mode "CMP")
;;     (define-key company-mode-map (kbd "M-/") 'company-complete)
;;     (define-key company-active-map (kbd "M-/") 'company-select-next)
;;     (define-key company-active-map (kbd "C-n") 'company-select-next)
;;     (define-key company-active-map (kbd "C-p") 'company-select-previous)
;;     ;; disable TAB in `company-mode', freeing it for YASnippet
;;     (define-key company-active-map [tab] nil)
;;     (define-key company-active-map (kbd "TAB") nil)
;;     (setq-default company-dabbrev-other-buffers 'all
;;                   company-tooltip-align-annotations t
;;                   company-tooltip-flip-when-above t
;;                   company-show-numbers t))
;;   (global-set-key (kbd "M-C-/") 'company-complete)
;;   (when (maybe-require-package 'company-quickhelp)
;;     (add-hook 'after-init-hook 'company-quickhelp-mode))

;;   (defun sanityinc/local-push-company-backend (backend)
;;     "Add BACKEND to a buffer-local version of `company-backends'."
;;     (make-local-variable 'company-backends)
;;     (push backend company-backends)))

;; ;; Suspend page-break-lines-mode while company menu is active
;; ;; (see https://github.com/company-mode/company-mode/issues/416)
;; (after-load 'company
;;   (after-load 'page-break-lines
;;     (defvar sanityinc/page-break-lines-on-p nil)
;;     (make-variable-buffer-local 'sanityinc/page-break-lines-on-p)

;;     (defun sanityinc/page-break-lines-disable (&rest ignore)
;;       (when (setq sanityinc/page-break-lines-on-p (bound-and-true-p page-break-lines-mode))
;;         (page-break-lines-mode -1)))

;;     (defun sanityinc/page-break-lines-maybe-reenable (&rest ignore)
;;       (when sanityinc/page-break-lines-on-p
;;         (page-break-lines-mode 1)))

;;     (add-hook 'company-completion-started-hook 'sanityinc/page-break-lines-disable)
;;     (add-hook 'company-completion-finished-hook 'sanityinc/page-break-lines-maybe-reenable)
;;     (add-hook 'company-completion-cancelled-hook 'sanityinc/page-break-lines-maybe-reenable)))
