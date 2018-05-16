;;; init-prog.el --- Basic programming settings      -*- lexical-binding: t; -*-
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

;; This file contains settings for basic (language-agnostic) programming.

;;; Code:

(use-package rainbow-delimiters         ; highlight delimiters by depth
  :ensure t
  :defer t
  :init
  (dolist (hook '(text-mode-hook prog-mode-hook))
    (add-hook hook #'rainbow-delimiters-mode)))

(use-package highlight-symbol           ; highlight current symbol
  :ensure t
  :defer t
  :bind
  (("M-s-n"   . highlight-symbol-next)
   ("M-s-p"   . highlight-symbol-prev)
   ("C-c s %" . highlight-symbol-query-replace)
   ("C-c s n" . highlight-symbol-next-in-defun)
   ("C-c s p" . highlight-symbol-prev-in-defun)
   ("C-c s o" . highlight-symbol-occur))
  :init
  ;; Navigate occurrences of the symbol under point with "M-n" and
  ;; "M-p", and highlight symbol occurrences
  (dolist (mode '(highlight-symbol-nav-mode highlight-symbol-mode))
    (add-hook 'prog-mode-hook mode))
  :config
  (validate-setq
   highlight-symbol-idle-delay 0.4      ; highlight almost immediately
   highlight-symbol-on-navigation-p t)  ; highlight immediately after navigation
  :diminish highlight-symbol-mode)

(use-package rainbow-mode ; fontify colors in buffers
  :ensure t
  :bind
  (("C-c t r" . rainbow-mode))
  :delight rainbow-mode
  :config
  (add-hook 'css-mode-hook #'rainbow-mode))

(use-package whitespace-cleanup-mode
  :ensure t
  :bind (("C-c x w" . whitespace-cleanup))
  :init (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
          (add-hook hook #'whitespace-cleanup-mode))
  :delight whitespace-cleanup-mode)

(use-package eldoc
  :ensure t
  :delight eldoc-mode)

(use-package yasnippet
  :commands (yas-reload-all yas-minor-mode)
  :delight yas-minor-mode " ⓨ "
  :init
  (add-hook 'prog-mode-hook
            (lambda () (yas-reload-all)(yas-minor-mode))))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

(provide 'init-prog)
;;; init-prog.el ends here
