;;; init-fonts.el --- Default font settings          -*- lexical-binding: t; -*-

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

;; This file contains font settings.

;;; Code:

;; Default fonts
(set-face-attribute 'default nil
                    :family "Iosevka Type" :height 120)

(set-face-attribute 'variable-pitch nil
                    :family "Fira Sans" :height 130 :weight 'regular)

(use-package face-remap			; face mapping
  :bind (("C-c w z" . text-scale-adjust)))

(provide 'init-fonts)
;;; init-fonts.el ends here
