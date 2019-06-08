;;; liskk-rule.el --- Misc rule for liskk      -*- lexical-binding: t; -*-

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
;; liskk-rule provide misc rule

;;; Code:

(require 'liskk-polyfill)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Definition
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Keyboard layout
;;

(defun liskk-rule-layout-make (layout)
  "Make liskk-rule from string LAYOUT."
  (mapcar (lambda (elm) (delq nil (mapcar #'kbd elm))) layout))

(defconst liskk-rule-us
  (liskk-rule-layout-make
   '(("`" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "-" "=")
     (    "q" "w" "e" "r" "t" "y" "u" "i" "o" "p" "[" "]" "\\")
     (    "a" "s" "d" "f" "g" "h" "j" "k" "l" ";" "'")
     (    "z" "x" "c" "v" "b" "n" "m" "," "." "/")))
  "The keyboard layout of US.")

(defconst liskk-rule-jis
  (liskk-rule-layout-make
   '(("`" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "-" "^" "¥")
     (    "q" "w" "e" "r" "t" "y" "u" "i" "o" "p" "@" "[")
     (    "a" "s" "d" "f" "g" "h" "j" "k" "l" ";" ":" "]")
     (    "z" "x" "c" "v" "b" "n" "m" "," "." "/" "\\")))
  "The keyboard layout of JIS.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Roman-kana conversion rule
;;

(defun liskk-rule-roman-make (tree &optional basestr)
  "Make roman rule from RULE."
  (let ((str     (concat basestr (pop tree)))
        (current (pop tree))
        (next    tree))
    (let ((vowel '("a" "i" "u" "e" "o"))
          (lst))
      (dolist (elm (number-sequence 0 4))
        (when (nth elm current)
          (push `(,(concat str (nth elm vowel)) nil ,(nth elm current)) lst)))
      (append (nreverse lst)
              (when next
                (mapcan
                 (lambda (elm)
                   (liskk-rule-roman-make elm str))
                 next))))))

(defun liskk-rule-roman-repeat-make (lst)
  "Make roman repeat rule."
  (mapcar
   (lambda (elm)
     `(,(concat elm elm) ,elm "っ"))
   lst))

(defun liskk-rule-roman-katakana-make (rule)
  "Make katakana rule from kana rule."
  (mapcar
   (lambda (elm)
     (let ((state     (pop elm))
           (nextstate (pop elm))
           (output    (pop elm)))
       (if (stringp output)
           `(,state ,nextstate (,output ,(japanese-katakana output)))
         `(,state ,nextstate ,output))))
   rule))

(defconst liskk-rule-roman-kana-base
  (liskk-rule-roman-katakana-make
   `(,@(liskk-rule-roman-make
        '(""     ("あ"   "い"   "う"   "え"   "お")
          ("k"   ("か"   "き"   "く"   "け"   "こ")
           ("y"  ("きゃ" "きぃ" "きゅ" "きぇ" "きょ")))
          ("g"   ("が"   "ぎ"   "ぐ"   "げ"   "ご")
           ("y"  ("ぎゃ" "ぎぃ" "ぎゅ" "ぎぇ" "ぎょ")))
          ("s"   ("さ"   "し"   "す"   "せ"   "そ")
           ("y"  ("しゃ" "しぃ" "しゅ" "しぇ" "しょ"))
           ("h"  ("しゃ" "し"   "しゅ" "しぇ" "しょ")))
          ("z"   ("ざ"   "じ"   "ず"   "ぜ"   "ぞ")
           ("y"  ("じゃ" "じぃ" "じゅ" "じぇ" "じょ")))
          ("j"   ("じゃ" "じ"   "じゅ" "じぇ" "じょ")
           ("y"  ("じゃ" "じぃ" "じゅ" "じぇ" "じょ")))
          ("t"   ("た"   "ち"   "つ"   "て"   "と")
           ("s"  (nil    nil    "つ"   nil    nil))
           ("y"  ("ちゃ" "ちぃ" "ちゅ" "ちぇ" "ちょ"))
           ("h"  ("てぁ" "てぃ" "てゅ" "てぇ" "てょ")))
          ("d"   ("だ"   "ぢ"   "づ"   "で"   "ど")
           ("h"  ("でゃ" "でぃ" "でゅ" "でぇ" "でょ"))
           ("y"  ("ぢゃ" "ぢぃ" "ぢゅ" "ぢぇ" "ぢょ")))
          ("c"   nil
           ("h"  ("ちゃ" "ち"   "ちゅ" "ちぇ" "ちょ"))
           ("y"  ("ちゃ" "ちぃ" "ちゅ" "ちぇ" "ちょ")))
          ("n"   ("な"   "に"   "ぬ"   "ね"   "の")
           ("y"  ("にゃ" "にぃ" "にゅ" "にぇ" "にょ")))
          ("h"   ("は"   "ひ"   "ふ"   "へ"   "ほ")
           ("y"  ("ひゃ" "ひぃ" "ひゅ" "ひゅ" "ひょ")))
          ("f"   ("ふぁ" "ふぃ" "ふ"   "ふぇ" "ふぉ")
           ("y"  ("ふゃ" "ふぃ" "ふゅ" "ふぇ" "ふょ")))
          ("b"   ("ば"   "び"   "ぶ"   "べ"   "ぼ")
           ("y"  ("びゃ" "びぃ" "びゅ" "びぇ" "びょ")))
          ("p"   ("ぱ"   "ぴ"   "ぷ"   "ぺ"   "ぽ")
           ("y"  ("ぴゃ" "ぴぃ" "ぴゅ" "ぴぇ" "ぴょ")))
          ("m"   ("ま"   "み"   "む"   "め"   "も")
           ("y"  ("みゃ" "みぃ" "みゅ" "みぇ" "みょ")))
          ("y"   ("や"   "い"   "ゆ"   "いぇ" "よ"))
          ("r"   ("ら"   "り"   "る"   "れ"   "ろ")
           ("y"  ("りゃ" "りぃ" "りゅ" "りぇ" "りょ")))
          ("w"   ("わ"   "うぃ" "う"   "うぇ" "を"))
          ("v"   ("ゔぁ" "ゔぃ" "ゔ"   "ゔぇ" "ゔぉ"))
          ("x"   ("ぁ"   "ぃ"   "ぅ"   "ぇ"   "ぉ")
           ("t"  (nil    nil    "っ"   nil    nil)
            ("s" (nil    nil    "っ"   nil    nil)))
           ("y"  ("ゃ"   nil    "ゅ"   nil    "ょ"))
           ("w"  ("ゎ"   "ゐ"   nil    "ゑ"    nil)))))
     ,@(liskk-rule-roman-repeat-make
        '("k" "g" "s" "z" "j" "t" "d" "c"
          "h" "f" "b" "p" "m" "y" "r" "w" "v" "x"))
     ("n" nil "ん") ("n'" nil "ん") ("nn" nil "ん"))))

(provide 'liskk-rule)
;;; liskk-rule.el ends here
