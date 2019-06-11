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

(require 'ov)


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  General overlay functions
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Make ov here functions
;;

(defun liskk-ov-here (point &rest properties)
  "Make an overlay on POINT with PROPERTIES.  see `ov'."
  (apply #'ov `(,point ,point ,@properties)))

(defalias 'liskk-ov-create-here 'liskk-ov-make-here)
(defun liskk-ov-make-here (point &optional buffer)
  "Create a new overlay at POINT in BUFFER and return it.  see `ov-make'."
  (apply #'ov-make `(,point ,point ,buffer)))

(defun liskk-ov-move-here (ov point &optional buffer shrink)
  "Set the endpoints of OV to POINTin BUFFER.  see `ov-move'.
If SHRINK is non-nil, OV will shrink 0 width."
  (apply #'ov-move
         `(,ov ,point ,(if shrink point (+ point (ov-length ov))) ,buffer)))

(defun liskk-ov-clear-here (point)
  "Clear overlays on POINT.  see `ov-clear'."
  (apply #'ov-clear `(,point ,point)))

(provide 'liskk-polyfill)
;;; liskk-polyfill.el ends here
