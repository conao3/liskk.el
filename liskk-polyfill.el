;;; liskk-polyfill.el --- Keyboard layout for liskk      -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Maintainer: Naoya Yamashita <conao3@gmail.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the Affero GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the Affero
;; GNU General Public License for more details.

;; You should have received a copy of the Affero GNU General Public
;; License along with this program.  If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;; liskk is yat another ddskk
;; liskk-polyfill provide keyboard translation

;;; Code:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  General list functions
;;

(defun leaf-mapcaappend (func seq &rest rest)
  "Another implementation for `mapcan' for FUNC SEQ REST.
`mapcan' uses `nconc', but Emacs-22 doesn't support it."
  (declare (indent 2))
  (apply #'append (apply #'mapcar func seq rest)))

(unless (fboundp 'mapcan)
  (defalias 'mapcan 'leaf-mapcaappend))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  General alist functions
;;

(defalias 'liskk-alist-get 'alist-get)

(provide 'liskk-polyfill)
;;; liskk-polyfill.el ends here
