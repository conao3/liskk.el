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

(cort-deftest liskk/rule
  '((:equal
     '(("a" nil "あ")
       ("i" nil "い")
       ("u" nil "う")
       ("e" nil "え")
       ("o" nil "お")
       ("ka" nil "か")
       ("ki" nil "き")
       ("ku" nil "く")
       ("ke" nil "け")
       ("ko" nil "こ")
       ("kya" nil "きゃ")
       ("kyi" nil "きぃ")
       ("kyu" nil "きゅ")
       ("kye" nil "きぇ")
       ("kyo" nil "きょ"))
     (liskk-rule-roman-make '(""
                              ("あ" "い" "う" "え" "お")
                              ("k"
                               ("か" "き" "く" "け" "こ")
                               ("y"
                                ("きゃ" "きぃ" "きゅ" "きぇ" "きょ")))
                              )))
    (:equal
     '(("a" nil "あ")
       ("i" nil "い")
       ("u" nil "う")
       ("e" nil "え")
       ("o" nil "お")
       ("ka" nil "か")
       ("ki" nil "き")
       ("ku" nil "く")
       ("ke" nil "け")
       ("ko" nil "こ")
       ("kya" nil "きゃ")
       ("kyi" nil "きぃ")
       ("kyu" nil "きゅ")
       ("kye" nil "きぇ")
       ("kyo" nil "きょ")
       ("cha" nil "ちゃ")
       ("chi" nil "ち")
       ("chu" nil "ちゅ")
       ("che" nil "ちぇ")
       ("cho" nil "ちょ")
       ("cya" nil "ちゃ")
       ("cyi" nil "ちぃ")
       ("cyu" nil "ちゅ")
       ("cye" nil "ちぇ")
       ("cyo" nil "ちょ")
       ("xa" nil "ぁ")
       ("xi" nil "ぃ")
       ("xu" nil "ぅ")
       ("xe" nil "ぇ")
       ("xo" nil "ぉ")
       ("xtu" nil "っ")
       ("xtsu" nil "っ")
       ("xya" nil "ゃ")
       ("xyu" nil "ゅ")
       ("xyo" nil "ょ")
       ("xwa" nil "ゎ")
       ("xwi" nil "ゐ")
       ("xwe" nil "ゑ"))
     (liskk-rule-roman-make '(""
                              ("あ" "い" "う" "え" "お")
                              ("k"
                               ("か" "き" "く" "け" "こ")
                               ("y"
                                ("きゃ" "きぃ" "きゅ" "きぇ" "きょ")))
                              ("c"
                               nil
                               ("h"
                                ("ちゃ" "ち" "ちゅ" "ちぇ" "ちょ"))
                               ("y"
                                ("ちゃ" "ちぃ" "ちゅ" "ちぇ" "ちょ")))
                              ("x"
                               ("ぁ" "ぃ" "ぅ" "ぇ" "ぉ")
                               ("t"
                                (nil nil "っ" nil nil)
                                ("s"
                                 (nil nil "っ" nil nil)))
                               ("y"
                                ("ゃ" nil "ゅ" nil "ょ"))
                               ("w"
                                ("ゎ" "ゐ" nil "ゑ" nil))))))
    (:equal
     '(("kk" "k" "っ") ("gg" "g" "っ") ("ss" "s" "っ"))
     (liskk-rule-roman-repeat-make '("k" "g" "s")))
    (:equal
     '(nil nil nil nil
           ((97 "a" nil "あ" nil)
            (105 "i" nil "い" nil)
            (117 "u" nil "う" nil)
            (101 "e" nil "え" nil)
            (111 "o" nil "お" nil)
            (107 "k" nil nil
                 ((107 "kk" "k" "っ" nil)
                  (97 "ka" nil "か" nil)
                  (105 "ki" nil "き" nil)
                  (117 "ku" nil "く" nil)
                  (101 "ke" nil "け" nil)
                  (111 "ko" nil "こ" nil)
                  (121 "y" nil nil
                       ((97 "kya" nil "きゃ" nil)
                        (105 "kyi" nil "きぃ" nil)
                        (117 "kyu" nil "きゅ" nil)
                        (101 "kye" nil "きぇ" nil)
                        (111 "kyo" nil "きょ" nil)))))
            (110 "n" nil "ん"
                 ((110 "nn" nil "ん" nil)
                  (97 "na" nil "な" nil)
                  (105 "ni" nil "に" nil)))))
     (let ((liskk-rule-tree '(nil nil nil nil nil)))
      (mapc
       (lambda (elm)
         (liskk-compile-rule-tree-add liskk-rule-tree (car elm) elm))
       (reverse '(("a" nil "あ")
                  ("i" nil "い")
                  ("u" nil "う")
                  ("e" nil "え")
                  ("o" nil "お")
                  ("kk" "k" "っ")
                  ("ka" nil "か")
                  ("ki" nil "き")
                  ("ku" nil "く")
                  ("ke" nil "け")
                  ("ko" nil "こ")
                  ("kya" nil "きゃ")
                  ("kyi" nil "きぃ")
                  ("kyu" nil "きゅ")
                  ("kye" nil "きぇ")
                  ("kyo" nil "きょ")
                  ("n" nil "ん")
                  ("nn" nil "ん")
                  ("na" nil "な")
                  ("ni" nil "に")
                  )))
      liskk-rule-tree))))

(provide 'leaf-keywords-tests)
;;; leaf-keywords-tests.el ends here
