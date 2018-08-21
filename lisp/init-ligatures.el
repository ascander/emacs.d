;;; init-ligatures.el --- Ligatures for use with Iosevka  -*- lexical-binding: t; -*-

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

;; Font ligatures setup

;;; Code:

;; I use Spacemacs, so I put this in user-config
;; Note that the script above only generates the long list of pairs.
;; The surrounding code is stolen from the PragmataPro scripts floating around on Gist.

(setq prettify-symbols-unprettify-at-point 'right-edge)

(defun ad|setup-iosevka-ligatures ()
  (setq prettify-symbols-alist
        (append prettify-symbols-alist '(
                                         ;; Equality operators ----------------
                                         ("==" . #Xe100)
                                         ("<=" . #Xe101)
                                         (">=" . #Xe102)
                                         ("!=" . #Xe103)
                                         ("===" . #Xe104)
                                         ("!==" . #Xe105)

                                         ;; Colons ----------------
                                         ("::" . #Xe146)
                                         (":::" . #Xe147)

                                         ;; Arrow-like operators ----------------
                                         ("->" . #Xe149)
                                         ("=>" . #Xe161)
                                         ("<-" . #Xe179)

                                         ;; Logical ----------------
                                         ("/\\" . #Xe1c7)
                                         ("\\/" . #Xe1c8)
                                         ))))

(defun ad|refresh-pretty ()
  (prettify-symbols-mode -1)
  (prettify-symbols-mode +1))

;; Hooks for modes in which to install the Iosevka ligatures
(mapc (lambda (hook)
        (add-hook hook (lambda () (ad|setup-iosevka-ligatures) (ad|refresh-pretty))))
      '(text-mode-hook
        prog-mode-hook))
(global-prettify-symbols-mode +1)

(provide 'init-ligatures)
;;; init-ligatures.el ends here
