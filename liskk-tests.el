;;; liskk-tests.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Maintainer: Naoya Yamashita <conao3@gmail.com>
;; URL: https://github.com/conao3/leaf.el

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

;;

;;; Code:

(load "cort-test")
(require 'liskk)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Test definition
;;

(cort-deftest liskk/layout-make
  '((:equal
     (liskk-rule-layout-make
      '(("`" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "-" "^" "¥")
        (    "q" "w" "e" "r" "t" "y" "u" "i" "o" "p" "@" "[")
        (    "a" "s" "d" "f" "g" "h" "j" "k" "l" ";" ":" "]")
        (    "z" "x" "c" "v" "b" "n" "m" "," "." "/" "\\")))
     '(("`" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "-" "^" [165])
       (    "q" "w" "e" "r" "t" "y" "u" "i" "o" "p" "@" "[")
       (    "a" "s" "d" "f" "g" "h" "j" "k" "l" ";" ":" "]")
       (    "z" "x" "c" "v" "b" "n" "m" "," "." "/" "\\")))))

(provide 'leaf-keywords-tests)
;;; leaf-keywords-tests.el ends here
