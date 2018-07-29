;;; init-markdown.el --- Part of my Emacs configuration  -*- lexical-binding: t; -*-

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Settings for working with Markdown documents

;;; Code:

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'"  . gfm-mode)
         ("\\.md\\'"        . markdown-mode)
         ("\\.markdown\\'"  . markdown-mode))
  :init
  (setq markdown-command "multimarkdown")
  :config
  (add-hook 'gfm-mode-hook #'turn-off-auto-fill) ; no filling in GFM
  (add-hook 'gfm-mode-hook #'visual-line-mode)   ; use visual lines instead

  (bind-key "C-c C-s C" #'markdown-insert-gfm-code-block markdown-mode-map)
  (bind-key "C-c C-s P" #'markdown-insert-gfm-code-block markdown-mode-map)

  ;; Stop me from myself
  (bind-key "M-q" #'ignore gfm-mode-map))

(provide 'init-markdown)
;;; init-markdown.el ends here
