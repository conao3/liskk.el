;;; liskk.el --- Yet another ddskk (Daredevil Simple Kana to Kanji conversion)      -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Maintainer: Naoya Yamashita <conao3@gmail.com>
;; Keywords: lisp settings
;; Version: 0.1.0
;; URL: https://github.com/conao3/liskk.el
;; Package-Requires: ((emacs "24.4"))

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

;;; Code:

(defgroup liskk nil
  "Yet another ddskk (Daredevil Simple Kana to Kanji conversion)."
  :group 'lisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Customization
;;

(defun liskk-rule-layout-make (layout)
  "Make liskk-rule from string LAYOUT."
  (mapcar (lambda (elm) (delq nil (mapcar #'kbd elm))) layout))

(defun liskk-rule-roman-make (rule)
  "Make roman rule from RULE."
  `(,(car rule) (,(cdr rule) (,(japanese-katakana (cdr rule))))))

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

(defconst liskk-rule-roman-kana-base-alist
  '(roman . (;; a
             ("a" nil "あ")
             ("i" nil "い")
             ("u" nil "う")
             ("e" nil "え")
             ("o" nil "お")

             ;; k
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
             ;; g
             ("gg" "g" "っ")
             ("ga" nil "が")
             ("gi" nil "ぎ")
             ("gu" nil "ぐ")
             ("ge" nil "げ")
             ("go" nil "ご")
             ("gya" nil "ぎゃ")
             ("gyi" nil "ぎぃ")
             ("gyu" nil "ぎゅ")
             ("gye" nil "ぎぇ")
             ("gyo" nil "ぎょ")
             
             ;; s
             ("ss" "s" "っ")
             ("sa" nil "さ")
             ("si" nil "し")
             ("shi" nil "し")
             ("su" nil "す")
             ("se" nil "せ")
             ("so" nil "そ")
             ("sya" nil "しゃ")
             ("syi" nil "しぃ")
             ("syu" nil "しゅ")
             ("sye" nil "しぇ")
             ("syo" nil "しょ")
             ("sha" nil "しゃ")
             ("shu" nil "しゅ")
             ("she" nil "しぇ")
             ("sho" nil "しょ")
             ;; z
             ("zz" "z" "っ")
             ("za" nil "ざ")
             ("zi" nil "じ")
             ("zu" nil "ず")
             ("ze" nil "ぜ")
             ("zo" nil "ぞ")
             ("zya" nil "じゃ")
             ("zyi" nil "じぃ")
             ("zyu" nil "じゅ")
             ("zye" nil "じぇ")
             ("zyo" nil "じょ")
             ;; j
             ("jj" "j" "っ")
             ("ja" nil "じゃ")
             ("ji" nil "じ")
             ("ju" nil "じゅ")
             ("je" nil "じぇ")
             ("jo" nil "じょ")
             ("jya" nil "じゃ")
             ("jyi" nil "じぃ")
             ("jyu" nil "じゅ")
             ("jye" nil "じぇ")
             ("jyo" nil "じょ")
             
             ;; t
             ("tt" "t" "っ")
             ("ta" nil "た")
             ("ti" nil "ち")
             ("tu" nil "つ")
             ("tsu" nil "つ")
             ("te" nil "て")
             ("to" nil "と")
             ("tha" nil "てぁ")
             ("thi" nil "てぃ")
             ("thu" nil "てゅ")
             ("the" nil "てぇ")
             ("tho" nil "てょ")
             ("tya" nil "ちゃ")
             ("tyi" nil "ちぃ")
             ("tyu" nil "ちゅ")
             ("tye" nil "ちぇ")
             ("tyo" nil "ちょ")
             ;; d
             ("dd" "d" "っ")
             ("da" nil "だ")
             ("di" nil "ぢ")
             ("du" nil "づ")
             ("de" nil "で")
             ("do" nil "ど")
             ("dha" nil "でゃ")
             ("dhi" nil "でぃ")
             ("dhe" nil "でぇ")
             ("dhu" nil "でゅ")
             ("dho" nil "でょ")
             ("dya" nil "ぢゃ")
             ("dyi" nil "ぢぃ")
             ("dyu" nil "ぢゅ")
             ("dye" nil "ぢぇ")
             ("dyo" nil "ぢょ")
             ;; c
             ("cc" "c" "っ")
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
             
             ;; n
             ;; ("n" nil "ん")
             ("n'" nil "ん")
             ("nn" nil "ん")
             ("na" nil "な")
             ("ni" nil "に")
             ("nu" nil "ぬ")
             ("ne" nil "ね")
             ("no" nil "の")
             ("nya" nil "にゃ")
             ("nyi" nil "にぃ")
             ("nyu" nil "にゅ")
             ("nye" nil "にぇ")
             ("nyo" nil "にょ")

             ;; h
             ;;("h" "" "お")
             ("hh" "h" "っ")
             ("ha" nil "は")
             ("hi" nil "ひ")
             ("hu" nil "ふ")
             ("he" nil "へ")
             ("ho" nil "ほ")
             ("hya" nil "ひゃ")
             ("hyi" nil "ひぃ")
             ("hyu" nil "ひゅ")
             ("hye" nil "ひぇ")
             ("hyo" nil "ひょ")
             ;; f
             ("ff" "f" "っ")
             ("fa" nil "ふぁ")
             ("fi" nil "ふぃ")
             ("fu" nil "ふ")
             ("fe" nil "ふぇ")
             ("fo" nil "ふぉ")
             ("fya" nil "ふゃ")
             ("fyi" nil "ふぃ")
             ("fyu" nil "ふゅ")
             ("fye" nil "ふぇ")
             ("fyo" nil "ふょ")
             ;; b
             ("bb" "b" "っ")
             ("ba" nil "ば")
             ("bi" nil "び")
             ("bu" nil "ぶ")
             ("be" nil "べ")
             ("bo" nil "ぼ")
             ("bya" nil "びゃ")
             ("byi" nil "びぃ")
             ("byu" nil "びゅ")
             ("bye" nil "びぇ")
             ("byo" nil "びょ")
             ;; p
             ("pp" "p" "っ")
             ("pa" nil "ぱ")
             ("pi" nil "ぴ")
             ("pu" nil "ぷ")
             ("pe" nil "ぺ")
             ("po" nil "ぽ")
             ("pya" nil "ぴゃ")
             ("pyi" nil "ぴぃ")
             ("pyu" nil "ぴゅ")
             ("pye" nil "ぴぇ")
             ("pyo" nil "ぴょ")
             
             ;; m
             ("mm" "m" "っ")
             ("ma" nil "ま")
             ("mi" nil "み")
             ("mu" nil "む")
             ("me" nil "め")
             ("mo" nil "も")
             ("mya" nil "みゃ")
             ("myi" nil "みぃ")
             ("myu" nil "みゅ")
             ("mye" nil "みぇ")
             ("myo" nil "みょ")
             
             ;; y
             ("yy" "y" "っ")
             ("ya" nil "や")
             ("yi" nil "ゐ")
             ("yu" nil "ゆ")
             ("ye" nil "いぇ")
             ("yo" nil "よ")

             ;; r
             ("rr" "r" "っ")
             ("ra" nil "ら")
             ("ri" nil "り")
             ("ru" nil "る")
             ("re" nil "れ")
             ("ro" nil "ろ")
             ("rya" nil "りゃ")
             ("ryi" nil "りぃ")
             ("ryu" nil "りゅ")
             ("rye" nil "りぇ")
             ("ryo" nil "りょ")
             
             ;; w
             ("ww" "w" "っ")
             ("wa" nil "わ")
             ("we" nil "うぇ")
             ("wi" nil "うぃ")
             ("wo" nil "を")
             ("wu" nil "う")

             ;; v
             ("vv" "v" "っ")
             ("va" nil "ゔぁ")
             ("ve" nil "ゔぇ")
             ("vi" nil "ゔぃ")
             ("vo" nil "ゔぉ")
             ("vu" nil "ゔ")

             ;; x
             ("xx" "x" "っ")
             ("xa" nil "ぁ")
             ("xi" nil "ぃ")
             ("xu" nil "ぅ")
             ("xe" nil "ぇ")
             ("xo" nil "ぉ")
             ("xtu" nil "っ")
             ("xtsu" nil "っ")
             ("xwa" nil "ゎ")
             ("xwi" nil "ゐ")
             ("xwe" nil "ゑ")
             ("xya" nil "ゃ")
             ("xyu" nil "ゅ")
             ("xyo" nil "ょ")

             ;; z
             ("z " nil "　")
             ("z*" nil "※")
             ("z," nil "‥")
             ("z-" nil "〜")
             ("z." nil "…")
             ("z/" nil "・")
             ("z0" nil "○")
             ("z:" nil "゜")
             ("z;" nil "゛")
             ("z@" nil "◎")
             ("z[" nil "『")
             ("z]" nil "』")
             ("z{" nil "【")
             ("z}" nil "】")
             ("z(" nil "（")
             ("z)" nil "）")
             ("zh" nil "←")
             ("zj" nil "↓")
             ("zk" nil "↑")
             ("zl" nil "→")
             ("zL" nil "⇒")

             ;; others
             ("." nil liskk-auto-kutouten)
             ("," nil liskk-auto-kutouten)
             ("-" nil liskk-auto-kutouten)
             (":" nil "：")
             (";" nil "；")
             ("?" nil "？")
             ("[" nil "「")
             ("]" nil "」")
             ("l" nil liskk-latin-mode)
             ("q" nil liskk-toggle-kana)
             ("L" nil liskk-jisx0208-latin-mode)
             ("Q" nil liskk-set-henkan-point-subr)
             ("X" nil liskk-purge-from-jisyo)
             ("/" nil liskk-abbrev-mode)
             ("$" nil liskk-display-code-for-char-at-point)
             ("@" nil liskk-today)
             ("\\" nil liskk-input-by-code-or-menu)
             (liskk-kakutei-key nil liskk-kakutei)))
  "The conversion rule roman to kana.

リストの各要素は次の形式である必要がある
  (INPUT-STATE NEXT-STATE OUTPUT)

INPUT-STATE, NEXT-STATEはstring, OUTPUTは下記の形式を指定できる
  String         - 全てのモードで指定された文字を挿入する
  List of String - モードによって挿入される文字を変更 (かなモード カナモード)
  Symbol         - 関数を実行する

INPUT-STATEを認識したら、OUTPUTを挿入/実行し、NEXT-STATEに移行する

Ex:
  (\"a\" nil (\"あ ア\"))
  (\"ki\" nil (\"き キ\"))
  (\"tt\" t (\"っ ッ\"))
  (\"q\" nil liskk-toggle-kana)

  a  => あ
  ki => き
  tt => っt
  q  => liskk-toggle-kana")

(defcustom liskk-rule-roman-kana-alist nil
  "Addtional conversionrule roman to kana.

*状態遷移規則のリストで、ユーザの追加設定用の変数。

この変数は、`liskk-rom-kana-base-rule-alist' と同様の書式を満たす必要がある。

LISKK は起動時にこの 2 変数を編集して `liskk-rule-tree' を作成するが、
`liskk-rule-roman-kana-alist' の規則は `liskk-rule-roman-kana-base-alist' の規則よりも
優先される。

リストの各要素は、それぞれが一つの規則であり、下記の形式を満たしていなければ
ならない。

 (INPUT-STATE NEXT-STATE OUTPUT)

LISKK は INPUT-STATE を検出すると、OUTPUT をバッファに挿入し、続いて
NEXT-STATE に状態を移したうえで、入力待ち状態となる。

詳しくは、`liskk-rule-roman-kana-base-alist' の説明を参照のこと。

ユーザは、追加したい規則を、例えば

    (setq liskk-rule-roman-kana-alist
      '((\"hh\" \"h\" (\"ッ\" . \"っ\"))
	(\"@\" nil \"＠\")
	...
	))

上記のように設定することができる。

この変数は、標準では

    (\"hh\" \"h\" (\"ッ\" . \"っ\"))

の設定がされている。この規則に従うと、

    ohhonn => おっほん
    ohhira => おっひら

のように挿入される。もしこれを

    ohhonn  => おおほん
    ohhira  => おおひら

のように変更したければ、この設定

    (\"hh\" \"h\" (\"ッ\" . \"っ\"))

を削除する。

また、`@' で `liskk-today' (当日の日付の入力) を起動する代りに `＠' を入
力したい場合は、`liskk-rule-roman-kana-alist' に

    (\"@\" nil \"＠\")

という要素を加える。

もし、LISKK を起動した後で `liskk-rule-roman-kana' を変更した場合、その設
定を反映させるには \\[liskk-restart] を実行する必要がある。"
  :type '(repeat
	  (list :tag "ルール"
		(radio :tag "1 入力"
		       (string :tag "文字列")
		       (symbol :tag "変数名"))
		(radio :tag "2 次の状態"
		       (string :tag "文字列")
		       (const :tag "nil (空の状態)" nil))
		(radio :tag "3 出力"
		       (function :tag "関数を実行する")
		       (string :tag "文字列")
		       (cons :tag "文字列の組"
			     (string :tag "3-1 カタカナ")
			     (string :tag "3-2 ひらがな")))))
  :group 'liskk-input-basic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Minor-mode
;;

(define-minor-mode liskk-mode
  "Yet another ddskk (Daredevil Simple Kana to Kanji conversion)."
  :require 'liskk
  :lighter " liskk"
  :group 'liskk)

(provide 'liskk)
;;; liskk.el ends here
